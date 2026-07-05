#include "truncation.hpp"

#include <algorithm>
#include <array>
#include <cstdlib>
#include <iostream>

#ifdef _OPENMP
#include <omp.h>
#endif

#include "transformations.hpp"

namespace FunKit
{
  namespace
  {
    // Contract a resolved metric factor: rename its closed index to the surviving open one, cf. the index
    // replacement in ReduceGamma (FEDeriK/Metric.m). If both gamma indices have the same position, the surviving
    // index switches position.
    void contract_gamma(FTerm &term, Idx gamma_idx)
    {
      const Idx i1 = term[gamma_idx].legs[0].second;
      const Idx i2 = term[gamma_idx].legs[1].second;
      const Idx flip = ((i1 < 0) == (i2 < 0)) ? -1 : 1;

      // Check whether the first index is contracted elsewhere in the term
      bool closed1 = false;
      for (Idx j = 0; j < Idx(term.size()) && !closed1; ++j) {
        if (j == gamma_idx) continue;
        for (const auto &leg : term[j].legs)
          if (std::abs(leg.second) == std::abs(i1)) {
            closed1 = true;
            break;
          }
      }

      const Idx from = closed1 ? i1 : i2;
      const Idx to = closed1 ? i2 : i1;
      for (Idx j = 0; j < Idx(term.size()); ++j) {
        if (j == gamma_idx) continue;
        for (auto &leg : term[j].legs)
          if (std::abs(leg.second) == std::abs(from)) leg.second = (leg.second < 0 ? -1 : 1) * flip * std::abs(to);
      }
    }

    // Index of the first 2pt correlation function with an AnyField leg, or -1 if there is none
    Idx first_open_2pt(const Setup &setup, const FTerm &term)
    {
      for (Idx i = 0; i < Idx(term.size()); ++i) {
        const auto &obj = term[i];
        if (setup.is_correlationFunction(obj.type) && obj.legs.size() == 2 &&
            (obj.legs[0].first == AnyField || obj.legs[1].first == AnyField))
          return i;
      }
      return -1;
    }

    // Expand the open 2pt function at first_2pt_idx over the truncation rules and push the pruned children
    // onto the worklist. Consumes the term.
    void expand_2pt(const Setup &setup, FTerm fterm, Idx first_2pt_idx, std::vector<FTerm> &work)
    {
      const auto &first_2pt = fterm[first_2pt_idx];

      // Get the cached truncation rules. A single empty rule means nothing is accepted at this order:
      // drop the term.
      const auto &type_rules = setup.truncation.truncation_rules(first_2pt.type, 2);
      if (!type_rules.empty() && type_rules.front().empty()) return;

      // An empty rule list means the type is unrestricted: expand over all field pairs
      const auto &rules = type_rules.empty() ? setup.truncation.all_field_pairs() : type_rules;

      // If exactly one leg is already specified, only rules matching it apply
      const std::array<bool, 2> anyfield_present = {first_2pt.legs[0].first == AnyField,
                                                    first_2pt.legs[1].first == AnyField};
      const bool filter = anyfield_present[0] != anyfield_present[1];
      const Idx spec_idx = anyfield_present[0] ? 1 : 0;
      const FieldIdx spec_field = first_2pt.legs[spec_idx].first;

      Idx n_applicable = 0;
      for (const auto &rule : rules)
        if (!filter || rule[spec_idx] == spec_field) ++n_applicable;
      if (n_applicable == 0) return; // no rule matches: the term is outside the truncation

      // The contracted indices of the 2pt function; saved before fterm is consumed below
      const Idx idx0 = std::abs(first_2pt.legs[0].second);
      const Idx idx1 = std::abs(first_2pt.legs[1].second);

      // Push the children in reverse rule order so the LIFO worklist processes them in rule order
      work.reserve(work.size() + n_applicable);
      for (Idx r = Idx(rules.size()) - 1; r >= 0; --r) {
        const auto &rule = rules[r];
        if (filter && rule[spec_idx] != spec_field) continue;

        // The last applicable rule consumes the term instead of copying it
        --n_applicable;
        FTerm child = (n_applicable == 0) ? std::move(fterm) : fterm;

        // Replace the legs of the 2pt function with the explicit fields from the truncation rule
        child[first_2pt_idx].legs[0].first = rule[0];
        child[first_2pt_idx].legs[1].first = rule[1];

        // Propagate the fields to all AnyField legs contracted with the 2pt function
        for (Idx i = 0; i < Idx(child.size()); ++i) {
          if (i == first_2pt_idx) continue;
          for (auto &leg : child[i].legs) {
            if (leg.first != AnyField) continue;
            if (std::abs(leg.second) == idx0)
              leg.first = rule[0];
            else if (std::abs(leg.second) == idx1)
              leg.first = rule[1];
          }
        }

        // Resolve FMinus and gamma factors with the now-explicit fields; vanished terms are dropped
        prune(setup, child);
        if (!child.empty()) work.push_back(std::move(child));
      }
    }

    // Truncate a single term, appending the finished terms to out. The worklist buffer is passed in so
    // that it can be reused across terms.
    void truncate_into(const Setup &setup, FTerm &&fterm, std::vector<FTerm> &work, FEq &out)
    {
      work.push_back(std::move(fterm));
      while (!work.empty()) {
        FTerm term = std::move(work.back());
        work.pop_back();

        const Idx first_2pt_idx = first_open_2pt(setup, term);
        if (first_2pt_idx == -1)
          out.push_back(std::move(term));
        else
          expand_2pt(setup, std::move(term), first_2pt_idx, work);
      }
    }
  } // namespace

  void prune(const Setup &setup, FTerm &term)
  {
    if (term.empty()) return;
    if (is_close(term.value, 0.)) {
      term.clear();
      return;
    }

    // We go object by object and check if we can apply truncation rules.
    bool changed = false;
    for (Idx oi = 0; oi < Idx(term.size()); ++oi) {
      Object &obj = term[oi];
      switch (obj.type) {
      case ObjectType::FMinus: {
        if (has_AnyField(obj)) break;
        if (setup.is_gField(obj.legs[0].first) && setup.is_gField(obj.legs[1].first)) term.value *= -1;
        obj.type = ObjectType::None;
        changed = true;
        break;
      }
      case ObjectType::gamma: {
        if (has_AnyField(obj)) break;
        term.value *= setup.gamma(obj.legs[0], obj.legs[1]);
        if (is_close(term.value, 0.)) {
          term.clear();
          return;
        }
        contract_gamma(term, oi);
        term[oi].type = ObjectType::None;
        changed = true;
        break;
      }
      }
    }
    // Only sweep out the resolved objects if anything was resolved
    if (changed) reduce(term);
  }

  void prune(const Setup &setup, FEq &feq)
  {
    bool any_empty = false;
    for (auto &term : feq) {
      prune(setup, term);
      any_empty |= term.empty();
    }
    if (any_empty) std::erase_if(feq, [](const FTerm &term) { return term.empty(); });
  }

  FEq truncate(const Setup &setup, FTerm fterm)
  {
    FEq out;
    std::vector<FTerm> work;
    truncate_into(setup, std::move(fterm), work, out);
    return out;
  }

  FEq &truncate(const Setup &setup, FEq &feq)
  {
    if (setup.debug_level > 0) std::cout << "\n===========> Truncating..." << std::endl;

    // One task per chunk of terms: each task truncates its terms serially via a chunk-local worklist,
    // which keeps the task granularity coarse enough to scale.
    constexpr Idx chunk_size = 1024;
    const Idx n_chunks = (Idx(feq.size()) + chunk_size - 1) / chunk_size;
    std::vector<FEq> results(n_chunks);

#pragma omp parallel shared(setup, feq, results) if (setup.debug_level <= 0)
#pragma omp single
    {
      for (Idx c = 0; c < n_chunks; ++c) {
#pragma omp task shared(setup, feq, results) firstprivate(c) if (setup.debug_level <= 0)
        {
          const Idx begin = c * chunk_size;
          const Idx end = std::min<Idx>(begin + chunk_size, Idx(feq.size()));
          std::vector<FTerm> work;
          for (Idx i = begin; i < end; ++i)
            truncate_into(setup, std::move(feq[i]), work, results[c]);
        }
      }
    }

    feq = merge_feq(std::move(results));

    return feq;
  }
} // namespace FunKit
