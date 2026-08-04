#pragma once

#include "DiFfRG/physics/interpolation.hh"
#include "DiFfRG/physics/physics.hh"

namespace DiFfRG {
  template<typename _Regulator>
  class ZA3_kernel
  {
    public:
    using Regulator = _Regulator;

    static KOKKOS_FORCEINLINE_FUNCTION auto kernel(const double& l1, const double& cos1, const double& cos2, const double& p, const double& k, const SplineInterpolator1D<double, LogarithmicCoordinates1D<double>, GPU_memory>& ZA3, const SplineInterpolator1D<double, LogarithmicCoordinates1D<double>, GPU_memory>& ZAcbc, const SplineInterpolator1D<double, LogarithmicCoordinates1D<double>, GPU_memory>& ZA4, const SplineInterpolator1D<double, LogarithmicCoordinates1D<double>, GPU_memory>& dtZc, const SplineInterpolator1D<double, LogarithmicCoordinates1D<double>, GPU_memory>& Zc, const SplineInterpolator1D<double, LogarithmicCoordinates1D<double>, GPU_memory>& dtZA, const SplineInterpolator1D<double, LogarithmicCoordinates1D<double>, GPU_memory>& ZA)
    {
      using namespace DiFfRG;using namespace DiFfRG::compute;
      const double cosl1p1 = cos1;
      const double cosl1p2 = 0.5 * (-cos1 + sqrt(3. - 3. * powr<2>(cos1)) * cos2);
      const double cosl1p3 = 0.5 * (-cos1 - sqrt(3. - 3. * powr<2>(cos1)) * cos2);
      // clang-format off
using _T = decltype(0.0909090909090909 * powr<-1>(1. + powr<6>(k)) * powr<-1>(p) * powr<-1>(powr<2>(l1) - 2. * cosl1p1 * l1 * p + powr<2>(p)) * powr<-1>(powr<2>(l1) - 2. * cosl1p1 * l1 * p - 2. * cosl1p2 * l1 * p + powr<2>(p)) * (-72. * powr<6>(l1) * p + 16. * powr<5>(cosl1p2) * powr<5>(l1) * powr<2>(p) - 372. * powr<4>(l1) * powr<3>(p) + 32. * powr<6>(cosl1p1) * powr<4>(l1) * powr<3>(p) - 378. * powr<2>(l1) * powr<5>(p) - 99. * powr<7>(p) + powr<5>(cosl1p1) * powr<3>(l1) * powr<2>(p) * (-208. * powr<2>(l1) + 96. * cosl1p2 * l1 * p - 448. * powr<2>(p)) + powr<4>(cosl1p2) * (-96. * powr<6>(l1) * p - 240. * powr<4>(l1) * powr<3>(p)) + powr<4>(cosl1p1) * powr<2>(l1) * p * (192. * powr<4>(l1) - 520. * cosl1p2 * powr<3>(l1) * p + (1244. + 40. * powr<2>(cosl1p2)) * powr<2>(l1) * powr<2>(p) - 1120. * cosl1p2 * l1 * powr<3>(p) + 1254. * powr<4>(p)) + powr<3>(cosl1p2) * (48. * powr<7>(l1) + 200. * powr<5>(l1) * powr<2>(p) + 128. * powr<3>(l1) * powr<4>(p)) + powr<2>(cosl1p2) * (24. * powr<6>(l1) * p + 56. * powr<4>(l1) * powr<3>(p) - 72. * powr<2>(l1) * powr<5>(p)) + powr<3>(cosl1p1) * l1 * (-48. * powr<6>(l1) + 384. * cosl1p2 * powr<5>(l1) * p + (-776. - 16. * powr<2>(cosl1p2)) * powr<4>(l1) * powr<2>(p) + cosl1p2 * (2488. - 80. * powr<2>(cosl1p2)) * powr<3>(l1) * powr<3>(p) + (-1920. - 544. * powr<2>(cosl1p2)) * powr<2>(l1) * powr<4>(p) + 2508. * cosl1p2 * l1 * powr<5>(p) - 924. * powr<6>(p)) + cosl1p2 * (144. * powr<5>(l1) * powr<2>(p) + 482. * powr<3>(l1) * powr<4>(p) + 231. * l1 * powr<6>(p)) + powr<2>(cosl1p1) * (132. * powr<6>(l1) * p + 326. * powr<4>(l1) * powr<3>(p) - 72. * powr<4>(cosl1p2) * powr<4>(l1) * powr<3>(p) + 153. * powr<2>(l1) * powr<5>(p) + 198. * powr<7>(p) + powr<3>(cosl1p2) * (496. * powr<5>(l1) * powr<2>(p) + 304. * powr<3>(l1) * powr<4>(p)) + powr<2>(cosl1p2) * (-144. * powr<6>(l1) * p + 436. * powr<4>(l1) * powr<3>(p) + 1302. * powr<2>(l1) * powr<5>(p)) + cosl1p2 * (-72. * powr<7>(l1) - 1164. * powr<5>(l1) * powr<2>(p) - 2880. * powr<3>(l1) * powr<4>(p) - 1386. * l1 * powr<6>(p))) + cosl1p1 * (288. * powr<5>(l1) * powr<2>(p) - 16. * powr<5>(cosl1p2) * powr<4>(l1) * powr<3>(p) + 964. * powr<3>(l1) * powr<4>(p) + 462. * l1 * powr<6>(p) + powr<4>(cosl1p2) * (232. * powr<5>(l1) * powr<2>(p) + 176. * powr<3>(l1) * powr<4>(p)) + powr<3>(cosl1p2) * (-336. * powr<6>(l1) * p - 808. * powr<4>(l1) * powr<3>(p) + 48. * powr<2>(l1) * powr<5>(p)) + powr<2>(cosl1p2) * (72. * powr<7>(l1) + 12. * powr<5>(l1) * powr<2>(p) - 704.0000000000001 * powr<3>(l1) * powr<4>(p) - 462. * l1 * powr<6>(p)) + cosl1p2 * (132. * powr<6>(l1) * p + 326. * powr<4>(l1) * powr<3>(p) + 153. * powr<2>(l1) * powr<5>(p) + 198. * powr<7>(p)))) * ((1. + 1. * powr<6>(k)) * dtZA(pow(1. + powr<6>(k),0.16666666666666666667)) * RB(powr<2>(k), powr<2>(l1)) + (1. + powr<6>(k)) * RBdot(powr<2>(k), powr<2>(l1)) * ZA(pow(1. + powr<6>(k),0.16666666666666666667)) + powr<6>(k) * RB(powr<2>(k), powr<2>(l1)) * (-50. * ZA(pow(1. + powr<6>(k),0.16666666666666666667)) + 50. * ZA(1.02 * pow(1. + powr<6>(k),0.16666666666666666667)))) * powr<-2>(RB(powr<2>(k), powr<2>(l1)) * ZA(pow(1. + powr<6>(k),0.16666666666666666667)) + powr<2>(l1) * ZA(l1)) * powr<-1>(RB(powr<2>(k), powr<2>(l1) - 2. * cosl1p1 * l1 * p + powr<2>(p)) * ZA(pow(1. + powr<6>(k),0.16666666666666666667)) + (powr<2>(l1) - 2. * cosl1p1 * l1 * p + powr<2>(p)) * ZA(sqrt(powr<2>(l1) - 2. * cosl1p1 * l1 * p + powr<2>(p)))) * powr<-1>(RB(powr<2>(k), powr<2>(l1) - 2. * (cosl1p1 + cosl1p2) * l1 * p + powr<2>(p)) * ZA(pow(1. + powr<6>(k),0.16666666666666666667)) + (powr<2>(l1) - 2. * cosl1p1 * l1 * p - 2. * cosl1p2 * l1 * p + powr<2>(p)) * ZA(sqrt(powr<2>(l1) - 2. * (cosl1p1 + cosl1p2) * l1 * p + powr<2>(p)))) * ZA3(0.816496580927726 * sqrt(powr<2>(l1) - cosl1p1 * l1 * p + powr<2>(p))) * ZA3(0.816496580927726 * sqrt(powr<2>(l1) - (cosl1p1 + cosl1p2) * l1 * p + powr<2>(p))) * ZA3(sqrt(0.6666666666666666 * powr<2>(l1) - 0.6666666666666666 * (2. * cosl1p1 + cosl1p2) * l1 * p + powr<2>(p))) - 0.2727272727272727 * powr<-1>(powr<2>(l1) - 2. * (cosl1p1 + cosl1p2) * l1 * p + powr<2>(p)) * ((-54. + 53. * powr<2>(cosl1p1) + 110. * cosl1p1 * cosl1p2 + 53. * powr<2>(cosl1p2)) * powr<2>(l1) - (-54. * cosl1p1 + 53. * powr<3>(cosl1p1) - 54. * cosl1p2 + 163. * powr<2>(cosl1p1) * cosl1p2 + 163. * cosl1p1 * powr<2>(cosl1p2) + 53. * powr<3>(cosl1p2)) * l1 * p + 33. * (-1. + powr<2>(cosl1p1) + 2. * cosl1p1 * cosl1p2 + powr<2>(cosl1p2)) * powr<2>(p)) * (RBdot(powr<2>(k), powr<2>(l1)) * ZA(pow(1. + powr<6>(k),0.16666666666666666667)) + RB(powr<2>(k), powr<2>(l1)) * (dtZA(pow(1. + powr<6>(k),0.16666666666666666667)) + 50. * powr<6>(k) * powr<-1>(1. + powr<6>(k)) * (-ZA(pow(1. + powr<6>(k),0.16666666666666666667)) + ZA(1.02 * pow(1. + powr<6>(k),0.16666666666666666667))))) * powr<-2>(RB(powr<2>(k), powr<2>(l1)) * ZA(pow(1. + powr<6>(k),0.16666666666666666667)) + powr<2>(l1) * ZA(l1)) * powr<-1>(RB(powr<2>(k), powr<2>(l1) - 2. * (cosl1p1 + cosl1p2) * l1 * p + powr<2>(p)) * ZA(pow(1. + powr<6>(k),0.16666666666666666667)) + (powr<2>(l1) - 2. * (cosl1p1 + cosl1p2) * l1 * p + powr<2>(p)) * ZA(sqrt(powr<2>(l1) - 2. * (cosl1p1 + cosl1p2) * l1 * p + powr<2>(p)))) * ZA3(0.816496580927726 * sqrt(powr<2>(l1) - (cosl1p1 + cosl1p2) * l1 * p + powr<2>(p))) * ZA4(0.5 * sqrt(2. * powr<2>(l1) - 2. * (cosl1p1 + cosl1p2) * l1 * p + 3. * powr<2>(p))) + 0.3636363636363637 * powr<2>(l1) * powr<-1>(p) * (1. * powr<3>(cosl1p1) * l1 - powr<3>(cosl1p2) * l1 + cosl1p1 * cosl1p2 * (-1.5 * cosl1p2 * l1 - 2.75 * p) + powr<2>(cosl1p1) * (1.5 * cosl1p2 * l1 - 2.75 * p) + 1.5 * p - 0.5 * powr<2>(cosl1p2) * p) * ZAcbc(0.816496580927726 * sqrt(powr<2>(l1) - cosl1p1 * l1 * p + powr<2>(p))) * ZAcbc(0.816496580927726 * sqrt(powr<2>(l1) - (cosl1p1 + cosl1p2) * l1 * p + powr<2>(p))) * ZAcbc(sqrt(0.6666666666666666 * powr<2>(l1) - 0.6666666666666666 * (2. * cosl1p1 + cosl1p2) * l1 * p + powr<2>(p))) * (dtZc(k) * RB(powr<2>(k), powr<2>(l1)) + RBdot(powr<2>(k), powr<2>(l1)) * Zc(k) + RB(powr<2>(k), powr<2>(l1)) * (-50. * Zc(k) + 50. * Zc(1.02 * k))) * powr<-2>(RB(powr<2>(k), powr<2>(l1)) * Zc(k) + powr<2>(l1) * Zc(l1)) * powr<-1>(RB(powr<2>(k), powr<2>(l1) - 2. * cosl1p1 * l1 * p + powr<2>(p)) * Zc(k) + (powr<2>(l1) - 2. * cosl1p1 * l1 * p + powr<2>(p)) * Zc(sqrt(powr<2>(l1) - 2. * cosl1p1 * l1 * p + powr<2>(p)))) * powr<-1>(RB(powr<2>(k), powr<2>(l1) - 2. * (cosl1p1 + cosl1p2) * l1 * p + powr<2>(p)) * Zc(k) + (powr<2>(l1) - 2. * cosl1p1 * l1 * p - 2. * cosl1p2 * l1 * p + powr<2>(p)) * Zc(sqrt(powr<2>(l1) - 2. * (cosl1p1 + cosl1p2) * l1 * p + powr<2>(p)))));
      // clang-format on
      const auto _interp1 = dtZA(pow(1. + powr<6>(k),0.16666666666666666667));
      const auto _interp2 = RB(powr<2>(k), powr<2>(l1));
      const auto _interp3 = RBdot(powr<2>(k), powr<2>(l1));
      const auto _interp4 = ZA(pow(1. + powr<6>(k),0.16666666666666666667));
      const auto _interp5 = ZA(1.02 * pow(1. + powr<6>(k),0.16666666666666666667));
      const auto _interp6 = ZA(l1);
      const auto _interp7 = RB(powr<2>(k), fma(-2., cosl1p1 * l1 * p, powr<2>(l1) + powr<2>(p)));
      const auto _interp9 = RB(powr<2>(k), fma(-2., (cosl1p1 + cosl1p2) * l1 * p, powr<2>(l1) + powr<2>(p)));
      const auto _interp10 = ZA(sqrt(fma(-2., (cosl1p1 + cosl1p2) * l1 * p, powr<2>(l1) + powr<2>(p))));
      const auto _interp12 = ZA3(0.816496580927726 * sqrt(fma(-1., (cosl1p1 + cosl1p2) * l1 * p, powr<2>(l1) + powr<2>(p))));
      const auto _den4 = powr<-2>(fma(_interp2, _interp4, fma(_interp6, powr<2>(l1), 0.)));_T _acc{};
      { // subkernel 1
        const auto _interp14 = ZA4(0.5 * sqrt(2. * powr<2>(l1) - 2. * (cosl1p1 + cosl1p2) * l1 * p + 3. * powr<2>(p)));
        const auto _interp15 = ZAcbc(0.816496580927726 * sqrt(powr<2>(l1) - cosl1p1 * l1 * p + powr<2>(p)));
        const auto _interp16 = ZAcbc(0.816496580927726 * sqrt(powr<2>(l1) - (cosl1p1 + cosl1p2) * l1 * p + powr<2>(p)));
        const auto _interp17 = ZAcbc(sqrt(0.6666666666666666 * powr<2>(l1) - 0.6666666666666666 * (2. * cosl1p1 + cosl1p2) * l1 * p + powr<2>(p)));
        const auto _interp18 = dtZc(k);
        const auto _interp19 = Zc(k);
        const auto _interp20 = Zc(1.02 * k);
        const auto _interp21 = Zc(l1);
        const auto _interp22 = Zc(sqrt(powr<2>(l1) - 2. * cosl1p1 * l1 * p + powr<2>(p)));
        const auto _interp23 = Zc(sqrt(powr<2>(l1) - 2. * (cosl1p1 + cosl1p2) * l1 * p + powr<2>(p)));
        const auto _den1 = powr<-1>(1. + powr<6>(k));
        const auto _den3 = powr<-2>(_interp19 * _interp2 + _interp21 * powr<2>(l1));
        const auto _den6 = powr<-1>(powr<2>(l1) - 2. * (cosl1p1 + cosl1p2) * l1 * p + powr<2>(p));
        const auto _den8 = powr<-1>(_interp19 * _interp7 + _interp22 * (powr<2>(l1) - 2. * cosl1p1 * l1 * p + powr<2>(p)));
        const auto _den10 = powr<-1>(_interp4 * _interp9 + _interp10 * (powr<2>(l1) - 2. * (cosl1p1 + cosl1p2) * l1 * p + powr<2>(p)));
        const auto _den12 = powr<-1>(_interp19 * _interp9 + _interp23 * (powr<2>(l1) + (-2. * cosl1p1 - 2. * cosl1p2) * l1 * p + powr<2>(p)));
        const auto _cse1 = -2.75 * p;_acc += fma(0.3636363636363637, _den12 * _den3 * _den8 * _interp15 * _interp16 * _interp17 * (_interp18 * _interp2 + _interp2 * (-50. * _interp19 + 50. * _interp20) + _interp19 * _interp3) * powr<2>(l1) * powr<-1>(p) * (1. * powr<3>(cosl1p1) * l1 - powr<3>(cosl1p2) * l1 + cosl1p1 * cosl1p2 * (_cse1 - 1.5 * cosl1p2 * l1) + powr<2>(cosl1p1) * (_cse1 + 1.5 * cosl1p2 * l1) + 1.5 * p - 0.5 * powr<2>(cosl1p2) * p), fma(-0.2727272727272727, _den10 * _den4 * _den6 * _interp12 * _interp14 * (_interp3 * _interp4 + _interp2 * (_interp1 + 50. * _den1 * (-_interp4 + _interp5) * powr<6>(k))) * ((-54. + 53. * powr<2>(cosl1p1) + 110. * cosl1p1 * cosl1p2 + 53. * powr<2>(cosl1p2)) * powr<2>(l1) - (-54. * cosl1p1 + 53. * powr<3>(cosl1p1) - 54. * cosl1p2 + 163. * powr<2>(cosl1p1) * cosl1p2 + 163. * cosl1p1 * powr<2>(cosl1p2) + 53. * powr<3>(cosl1p2)) * l1 * p + 33. * (-1. + powr<2>(cosl1p1) + 2. * cosl1p1 * cosl1p2 + powr<2>(cosl1p2)) * powr<2>(p)), 0.));
      }
      { // subkernel 2
        const auto _interp8 = ZA(sqrt(powr<2>(l1) - 2. * cosl1p1 * l1 * p + powr<2>(p)));
        const auto _interp11 = ZA3(0.816496580927726 * sqrt(powr<2>(l1) - cosl1p1 * l1 * p + powr<2>(p)));
        const auto _interp13 = ZA3(sqrt(0.6666666666666666 * powr<2>(l1) - 0.6666666666666666 * (2. * cosl1p1 + cosl1p2) * l1 * p + powr<2>(p)));
        const auto _den2 = powr<-1>(1. + powr<6>(k));
        const auto _den5 = powr<-1>(powr<2>(l1) - 2. * cosl1p1 * l1 * p + powr<2>(p));
        const auto _den7 = powr<-1>(powr<2>(l1) + (-2. * cosl1p1 - 2. * cosl1p2) * l1 * p + powr<2>(p));
        const auto _den9 = powr<-1>(_interp4 * _interp7 + _interp8 * (powr<2>(l1) - 2. * cosl1p1 * l1 * p + powr<2>(p)));
        const auto _den11 = powr<-1>(_interp4 * _interp9 + _interp10 * (powr<2>(l1) + (-2. * cosl1p1 - 2. * cosl1p2) * l1 * p + powr<2>(p)));
        const auto _cse1 = 132. * powr<6>(l1) * p;
        const auto _cse2 = 326. * powr<4>(l1) * powr<3>(p);
        const auto _cse3 = 153. * powr<2>(l1) * powr<5>(p);
        const auto _cse4 = 198. * powr<7>(p);// clang-format off
        _acc += 0.0909090909090909 * _den11 * _den2 * _den4 * _den5 * _den7 * _den9 * _interp11 * _interp12 * _interp13 * powr<-1>(p) * fma(-72., powr<6>(l1) * p, fma(16., powr<5>(cosl1p2) * powr<5>(l1) * powr<2>(p), fma(-372., powr<4>(l1) * powr<3>(p), fma(32., powr<6>(cosl1p1) * powr<4>(l1) * powr<3>(p), fma(-378., powr<2>(l1) * powr<5>(p), fma(-99., powr<7>(p), fma(powr<5>(cosl1p1), powr<3>(l1) * powr<2>(p) * (-208. * powr<2>(l1) + 96. * cosl1p2 * l1 * p - 448. * powr<2>(p)), fma(powr<4>(cosl1p2), -96. * powr<6>(l1) * p - 240. * powr<4>(l1) * powr<3>(p), fma(powr<4>(cosl1p1), powr<2>(l1) * p * (192. * powr<4>(l1) - 520. * cosl1p2 * powr<3>(l1) * p + (1244. + 40. * powr<2>(cosl1p2)) * powr<2>(l1) * powr<2>(p) - 1120. * cosl1p2 * l1 * powr<3>(p) + 1254. * powr<4>(p)), fma(powr<3>(cosl1p2), 48. * powr<7>(l1) + 200. * powr<5>(l1) * powr<2>(p) + 128. * powr<3>(l1) * powr<4>(p), fma(powr<2>(cosl1p2), 24. * powr<6>(l1) * p + 56. * powr<4>(l1) * powr<3>(p) - 72. * powr<2>(l1) * powr<5>(p), fma(powr<3>(cosl1p1), l1 * (-48. * powr<6>(l1) + 384. * cosl1p2 * powr<5>(l1) * p + (-776. - 16. * powr<2>(cosl1p2)) * powr<4>(l1) * powr<2>(p) + cosl1p2 * (2488. - 80. * powr<2>(cosl1p2)) * powr<3>(l1) * powr<3>(p) + (-1920. - 544. * powr<2>(cosl1p2)) * powr<2>(l1) * powr<4>(p) + 2508. * cosl1p2 * l1 * powr<5>(p) - 924. * powr<6>(p)), fma(cosl1p2, 144. * powr<5>(l1) * powr<2>(p) + 482. * powr<3>(l1) * powr<4>(p) + 231. * l1 * powr<6>(p), fma(powr<2>(cosl1p1), _cse1 + _cse2 + _cse3 + _cse4 - 72. * powr<4>(cosl1p2) * powr<4>(l1) * powr<3>(p) + powr<3>(cosl1p2) * (496. * powr<5>(l1) * powr<2>(p) + 304. * powr<3>(l1) * powr<4>(p)) + powr<2>(cosl1p2) * (-144. * powr<6>(l1) * p + 436. * powr<4>(l1) * powr<3>(p) + 1302. * powr<2>(l1) * powr<5>(p)) + cosl1p2 * (-72. * powr<7>(l1) - 1164. * powr<5>(l1) * powr<2>(p) - 2880. * powr<3>(l1) * powr<4>(p) - 1386. * l1 * powr<6>(p)), fma(cosl1p1, (_cse1 + _cse2 + _cse3 + _cse4) * cosl1p2 + 288. * powr<5>(l1) * powr<2>(p) - 16. * powr<5>(cosl1p2) * powr<4>(l1) * powr<3>(p) + 964. * powr<3>(l1) * powr<4>(p) + 462. * l1 * powr<6>(p) + powr<4>(cosl1p2) * (232. * powr<5>(l1) * powr<2>(p) + 176. * powr<3>(l1) * powr<4>(p)) + powr<3>(cosl1p2) * (-336. * powr<6>(l1) * p - 808. * powr<4>(l1) * powr<3>(p) + 48. * powr<2>(l1) * powr<5>(p)) + powr<2>(cosl1p2) * (72. * powr<7>(l1) + 12. * powr<5>(l1) * powr<2>(p) - 704.0000000000001 * powr<3>(l1) * powr<4>(p) - 462. * l1 * powr<6>(p)), 0.))))))))))))))) * fma(_interp2, (-50. * _interp4 + 50. * _interp5) * powr<6>(k), fma(_interp3, _interp4 * (1. + powr<6>(k)), fma(_interp1, _interp2 * (1. + 1. * powr<6>(k)), 0.)));
        // clang-format on

      }
      return _acc;
    }

    static KOKKOS_FORCEINLINE_FUNCTION auto constant(const double& p, const double& k, const SplineInterpolator1D<double, LogarithmicCoordinates1D<double>, GPU_memory>& ZA3, const SplineInterpolator1D<double, LogarithmicCoordinates1D<double>, GPU_memory>& ZAcbc, const SplineInterpolator1D<double, LogarithmicCoordinates1D<double>, GPU_memory>& ZA4, const SplineInterpolator1D<double, LogarithmicCoordinates1D<double>, GPU_memory>& dtZc, const SplineInterpolator1D<double, LogarithmicCoordinates1D<double>, GPU_memory>& Zc, const SplineInterpolator1D<double, LogarithmicCoordinates1D<double>, GPU_memory>& dtZA, const SplineInterpolator1D<double, LogarithmicCoordinates1D<double>, GPU_memory>& ZA)
    {
      using namespace DiFfRG;using namespace DiFfRG::compute;
      return 0.;
    }
    private:
    static KOKKOS_FORCEINLINE_FUNCTION auto RB(const auto& k2, const auto& p2)
    {
      return Regulator::RB(k2, p2);
    }

    static KOKKOS_FORCEINLINE_FUNCTION auto RF(const auto& k2, const auto& p2)
    {
      return Regulator::RF(k2, p2);
    }

    static KOKKOS_FORCEINLINE_FUNCTION auto RBdot(const auto& k2, const auto& p2)
    {
      return Regulator::RBdot(k2, p2);
    }

    static KOKKOS_FORCEINLINE_FUNCTION auto RFdot(const auto& k2, const auto& p2)
    {
      return Regulator::RFdot(k2, p2);
    }

    static KOKKOS_FORCEINLINE_FUNCTION auto dq2RB(const auto& k2, const auto& p2)
    {
      return Regulator::dq2RB(k2, p2);
    }

    static KOKKOS_FORCEINLINE_FUNCTION auto dq2RF(const auto& k2, const auto& p2)
    {
      return Regulator::dq2RF(k2, p2);
    }
  };
} using DiFfRG::ZA3_kernel;