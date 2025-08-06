#set page("a4", margin: 12pt)

#let vu(x) = [$bold(upright(#x))$]
#let vuh(x) = [$hat(vu(#x))$]

#let k = $1/(4 pi epsilon_0)$
#let k_mag = $mu_0/(4pi)$

#let curly_r = $cal(hat(bold(r)))$

#let parts = (
  vre: grid.cell(colspan: 2)[
    $V,vu(E),rho$
    #table(
      columns: 3,
      column-gutter: 10pt,
      stroke: none,
      table.header()[$V<->rho$][$vu(E)<->rho$][$vu(E)<->V$],
      table.hline(start: 0),
      $V=#k integral rho/cal(r)$,
      $vu(E)=#k integral #curly_r/cal(r^2) rho d tau$,
      $vu(E)=-gradient V$,
      $gradient^2 V = -rho/epsilon_0$,
      $rho/epsilon_0 = gradient dot vu(E)$,
      $V = -integral vu(E) dot d vu(l)$,
    )
    $V(vu(r)) = #k integral rho(vu(r')) / #curly_r d tau'$
  ],
  work: [Work

    $W = epsilon_0/2 integral_"all space" E^2 d tau$

    $W = 1/2 integral rho V d tau$],
  dipole: [Dipole

    $vu(p) = q vu(d) = integral vu(r') rho(vu(r')) d tau$

    $V_"dip"(r, theta) = #k (hat(vu(r)) dot vu(p)) / r^2$

    $E_"dip"(r, theta) &= #k 1/r^3 (2 cos theta hat(vu(r)) + sin theta hat(vu(theta))) \
    &=#k 1/r^3(3(vu(p) dot hat(vu(r)))hat(vu(r)) - vu(p))$

  ],
  magneto: [Magnetostatics

    $vu(F) = Q[vu(E) + (vu(v) times vu(B))]$

    $vu(B)(vu(r)) = #k_mag I integral (d vu(l)' times #curly_r)/cal(r)^2$


    $B = (mu_0 I) / (2 pi s)$

    $vu(K) = (d vu(I)) / (d l_bot) = sigma vu(v); vu(J) = (d vu(I)) / (d a_bot) = rho vu(v)$

    $gradient dot vu(J) = - (partial p) / (partial t) (= 0 "in steady current")$

    $gradient times vu(B) = mu_0 vu(J)$;$" "$ $integral.cont vu(B) dot d vu(l) = mu_0 I_"enc"$

    From a segment of wire:

    $B = (mu_0 I)/(4 pi s)(sin theta_2 - sin theta_1)$

    $vu(B) = gradient times vu(A)$; $gradient dot vu(A) = 0$

    $gradient^2 vu(A) = -mu_0 vu(J)$

    Magnetic Dipole

    $vu(m) = I integral d vu(a') = I vu(a)$

    $vu(A_"dip") = #k_mag (vu(m) times #curly_r) / cal(r)^2$

    $B_"dip" = #k_mag (1/r^3) [3(vu(m) dot vuh(r)) vuh(r) - vu(m)]$

  ],
  cap: [Capacitance

    $C = Q/V = (A epsilon_0) / d$

    $V = vu(E) d$

    $W = 1/2 C V^2 = Q^2/(2C)$
  ],
  polar: grid.cell(rowspan: 2)[Dielectrics (evil chapter 4)

    $E_e = #k q d / a^3, p = q d = (4 pi epsilon_0 a^3) E$

    $alpha = 4 pi epsilon_0 a^3 = 3 epsilon_0 v$

    $vu(N) = vu(p) times vu(E)$

    $vu(F) = (vu(p) dot gradient) vu(E)$

    $sigma_b equiv vu(P) dot vuh(n), rho_b equiv - gradient dot vu(P)$

    $V(vu(r)) = #k integral.cont_cal(S) sigma_b / cal(r) d a' + #k integral_cal(V) rho_b / cal(r) d tau'$

    $vu(D) equiv epsilon_0 vu(E) + vu(P)$

    $gradient dot vu(D) = rho_f, integral.cont vu(D) dot d vu(a) = Q_f_"enc"$

    $W = 1/2 integral vu(D) dot vu(E) d tau$

    Linear:

    $vu(P) = epsilon_0 chi_e vu(E)$

    $vu(D) = epsilon vu(E), epsilon = epsilon_0 (1+chi_e) = epsilon_0 epsilon_r$

    $C = epsilon_r C_"vac"$

    $W = epsilon_r W_"vac"$

  ],
  surface: [Surface

    $vu(E) = sigma / epsilon_0 vuh(n)$

    $sigma = -epsilon_0 (partial V) / (partial n)$

    $vu(f) = sigma vu(E)_"avg" = 1/(2 epsilon_0) sigma^2 vuh(n)$

    $rho = epsilon_0/2 E^2$
  ],
  maths: [Maths #emoji.face.vomit
    #include "coord_formulae.typ"
  ],
)

#grid(
  gutter: 25pt,
  columns: 3,
  parts.vre,
  parts.surface,
  parts.work,
  parts.dipole,
  parts.cap,
  parts.maths,

  parts.polar,
  parts.magneto,
)
