#include "truncation.hpp"

#include <algorithm>
#include <array>
#include <cstdlib>
#include <iostream>
#include <limits>

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

    // Index of the open correlation function or Field object with the fewest legs, or -1 if
    // there is none. Picking the minimal arity walks the truncation order by order: all open
    // Field/1pt objects are expanded before any 2pt, all 2pt before any 3pt, and so on.
    // Expansions never create new AnyField legs, so lower orders cannot reappear once they are
    // exhausted. Lower orders have fewer rules, so this keeps the branching narrow and drops
    // out-of-truncation terms early.
    Idx first_open_correlator(const Setup &setup, const FTerm &term)
    {
      Idx best = -1;
      size_t best_arity = std::numeric_limits<size_t>::max();
      for (Idx i = 0; i < Idx(term.size()); ++i) {
        const auto &obj = term[i];
        if (!(setup.is_correlationFunction(obj.type) || obj.type == ObjectType::Field) || !has_AnyField(obj)) continue;
        if (obj.legs.size() < best_arity) {
          best = i;
          best_arity = obj.legs.size();
          if (best_arity == 1) break;
        }
      }
      return best;
    }

    // Assign the fields (one per leg) to the correlation function at obj_idx, propagate each field
    // to every AnyField leg elsewhere in the term that is contracted with the corresponding leg
    // index (the first matching leg wins on repeated indices), prune, and push the child onto the
    // worklist if it survived. leg_sidx holds the abs leg indices, saved before the term is consumed.
    void emit_child(const Setup &setup, FTerm &&child, Idx obj_idx, const gch::small_vector<Idx, 4> &leg_sidx,
                    const std::vector<FieldIdx> &fields, std::vector<FTerm> &work)
    {
      auto &legs = child[obj_idx].legs;
      for (Idx j = 0; j < Idx(legs.size()); ++j)
        legs[j].first = fields[j];

      for (Idx i = 0; i < Idx(child.size()); ++i) {
        if (i == obj_idx) continue;
        for (auto &leg : child[i].legs) {
          if (leg.first != AnyField) continue;
          for (Idx j = 0; j < Idx(leg_sidx.size()); ++j) {
            if (std::abs(leg.second) != leg_sidx[j]) continue;
            leg.first = fields[j];
            break;
          }
        }
      }

      // Resolve FMinus and gamma factors with the now-explicit fields; vanished terms are dropped
      prune(setup, child);
      if (!child.empty()) work.push_back(std::move(child));
    }

    // Expand the open 1pt function or Field object at obj_idx over the order-1 truncation rules
    // and push the pruned children onto the worklist. Consumes the term.
    void expand_1pt(const Setup &setup, FTerm fterm, Idx obj_idx, std::vector<FTerm> &work)
    {
      const auto &obj = fterm[obj_idx];

      // A single empty rule means nothing is accepted at this order: drop the term
      const auto &type_rules = setup.truncation.truncation_rules(obj.type, 1);
      if (!type_rules.empty() && type_rules.front().empty()) return;

      const gch::small_vector<Idx, 4> leg_sidx = {std::abs(obj.legs[0].second)};

      // An empty rule list means the type is unrestricted: expand over all fields. The open 1pt
      // has its single leg AnyField, so no concrete-leg filter is needed in either branch.
      // Push the children in reverse order so the LIFO worklist processes them in rule order.
      if (type_rules.empty()) {
        const auto fields = setup.all_fields();
        work.reserve(work.size() + fields.size());
        for (Idx r = Idx(fields.size()) - 1; r >= 0; --r) {
          FTerm child = (r == 0) ? std::move(fterm) : fterm;
          emit_child(setup, std::move(child), obj_idx, leg_sidx, {fields[r]}, work);
        }
        return;
      }

      work.reserve(work.size() + type_rules.size());
      for (Idx r = Idx(type_rules.size()) - 1; r >= 0; --r) {
        FTerm child = (r == 0) ? std::move(fterm) : fterm;
        emit_child(setup, std::move(child), obj_idx, leg_sidx, type_rules[r], work);
      }
    }

    // Expand the open 2pt function at obj_idx over the order-2 truncation rules and push the
    // pruned children onto the worklist. Consumes the term.
    void expand_2pt(const Setup &setup, FTerm fterm, Idx obj_idx, std::vector<FTerm> &work)
    {
      const auto &first_2pt = fterm[obj_idx];

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
      const gch::small_vector<Idx, 4> leg_sidx = {std::abs(first_2pt.legs[0].second),
                                                  std::abs(first_2pt.legs[1].second)};

      // Push the children in reverse rule order so the LIFO worklist processes them in rule order
      work.reserve(work.size() + n_applicable);
      for (Idx r = Idx(rules.size()) - 1; r >= 0; --r) {
        const auto &rule = rules[r];
        if (filter && rule[spec_idx] != spec_field) continue;

        // The last applicable rule consumes the term instead of copying it
        --n_applicable;
        FTerm child = (n_applicable == 0) ? std::move(fterm) : fterm;
        emit_child(setup, std::move(child), obj_idx, leg_sidx, rule, work);
      }
    }

    // Expand the open n-leg (n >= 3) correlation function at obj_idx over the order-n truncation
    // rules and push the pruned children onto the worklist. Consumes the term.
    void expand_npt(const Setup &setup, FTerm fterm, Idx obj_idx, std::vector<FTerm> &work)
    {
      const auto &obj = fterm[obj_idx];
      const Idx n_legs = Idx(obj.legs.size());

      // A single empty rule means nothing is accepted at this order: drop the term. This also
      // covers restricted types whose rules only exist at other orders.
      const auto &type_rules = setup.truncation.truncation_rules(obj.type, n_legs);
      if (!type_rules.empty() && type_rules.front().empty()) return;

      // The leg indices and fields, saved before fterm is consumed below
      gch::small_vector<Idx, 4> leg_sidx;
      gch::small_vector<FieldIdx, 4> leg_fields;
      for (const auto &leg : obj.legs) {
        leg_sidx.push_back(std::abs(leg.second));
        leg_fields.push_back(leg.first);
      }

      // An empty rule list means the type is unrestricted: expand the still-open legs over all
      // field combinations, keeping the already-concrete legs fixed
      if (type_rules.empty()) {
        const auto fields = setup.all_fields();
        gch::small_vector<Idx, 4> open; // positions of the AnyField legs
        std::vector<FieldIdx> assignment(n_legs);
        for (Idx j = 0; j < n_legs; ++j) {
          if (leg_fields[j] == AnyField)
            open.push_back(j);
          else
            assignment[j] = leg_fields[j];
        }

        Idx n_children = 1;
        for (size_t k = 0; k < open.size(); ++k)
          n_children *= Idx(fields.size());
        work.reserve(work.size() + n_children);

        // Count the odometer down so the children are pushed in reverse enumeration order and the
        // LIFO worklist processes them in field order, matching the rule-order convention above
        std::vector<Idx> digits(open.size(), Idx(fields.size()) - 1);
        for (Idx c = n_children - 1; c >= 0; --c) {
          for (size_t k = 0; k < open.size(); ++k)
            assignment[open[k]] = fields[digits[k]];
          FTerm child = (c == 0) ? std::move(fterm) : fterm;
          emit_child(setup, std::move(child), obj_idx, leg_sidx, assignment, work);
          for (Idx k = Idx(open.size()) - 1; k >= 0; --k) {
            if (digits[k] > 0) {
              --digits[k];
              break;
            }
            digits[k] = Idx(fields.size()) - 1;
          }
        }
        return;
      }

      // Only rules matching all already-concrete legs apply
      const auto matches = [&](const std::vector<FieldIdx> &rule) {
        for (Idx j = 0; j < n_legs; ++j)
          if (leg_fields[j] != AnyField && rule[j] != leg_fields[j]) return false;
        return true;
      };

      Idx n_applicable = 0;
      for (const auto &rule : type_rules)
        if (matches(rule)) ++n_applicable;
      if (n_applicable == 0) return; // no rule matches: the term is outside the truncation

      // Push the children in reverse rule order so the LIFO worklist processes them in rule order
      work.reserve(work.size() + n_applicable);
      for (Idx r = Idx(type_rules.size()) - 1; r >= 0; --r) {
        const auto &rule = type_rules[r];
        if (!matches(rule)) continue;

        // The last applicable rule consumes the term instead of copying it
        --n_applicable;
        FTerm child = (n_applicable == 0) ? std::move(fterm) : fterm;
        emit_child(setup, std::move(child), obj_idx, leg_sidx, rule, work);
      }
    }

    // Truncate a single term, appending the finished terms to out. The worklist buffer is passed in so
    // that it can be reused across terms. Each round expands the open correlation function of the
    // lowest order, so a term is resolved order by order until no AnyField correlator remains.
    void truncate_into(const Setup &setup, FTerm &&fterm, std::vector<FTerm> &work, FEq &out)
    {
      // Expansion children are pruned in emit_child; pruning the incoming term here guarantees
      // that terms needing no expansion also come out resolved and inside the truncation
      prune(setup, fterm);
      if (fterm.empty()) return;

      work.push_back(std::move(fterm));
      while (!work.empty()) {
        FTerm term = std::move(work.back());
        work.pop_back();

        const Idx obj_idx = first_open_correlator(setup, term);
        if (obj_idx == -1) {
          out.push_back(std::move(term));
          continue;
        }

        switch (term[obj_idx].legs.size()) {
        case 1:
          expand_1pt(setup, std::move(term), obj_idx, work);
          break;
        case 2:
          expand_2pt(setup, std::move(term), obj_idx, work);
          break;
        default:
          expand_npt(setup, std::move(term), obj_idx, work);
          break;
        }
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
      case ObjectType::SymmFactor: {
        if (has_AnyField(obj)) break;
        // 1/n! for every group of identical fields, cf. SymmetryFactorFromList (FEDeriK/Metric.m)
        gch::small_vector<FieldIdx, 4> fields;
        for (const auto &leg : obj.legs)
          fields.push_back(leg.first);
        std::sort(fields.begin(), fields.end());
        for (size_t i = 0; i < fields.size();) {
          size_t j = i + 1;
          while (j < fields.size() && fields[j] == fields[i])
            ++j;
          for (size_t n = 2; n <= j - i; ++n)
            term.value /= double(n);
          i = j;
        }
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
      default: {
        if (has_AnyField(obj)) break;
        if (!setup.truncation.in_truncation(obj)) {
          term.clear();
          return;
        }
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
    normalize(setup, out);
    return out;
  }

  void truncate(const Setup &setup, FEq &feq)
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
          normalize(setup, results[c]);
        }
      }
    }

    feq = merge_feq(std::move(results));
  }
} // namespace FunKit
