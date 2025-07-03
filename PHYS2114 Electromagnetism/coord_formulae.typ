#let vu(x) = [$bold(upright(#x))$]
#let vuc(x) = [$bold(#x)$]
#let vuh(x) = [$hat(vuc(#x))$]



#text(size: 9pt)[
=== Cartesian
$nabla t = frac(diff t, diff x) vuh(x) + frac(diff t, diff y) vuh(y) + frac(diff t, diff z) vuh(z) $

$nabla dot.op vuc(v) = frac(diff v_x, diff x) + frac(diff v_y, diff y) + frac(diff v_z, diff z) $

$nabla times vuc(v) = (frac(diff v_z, diff y) - frac(diff v_y, diff z)) vuh(x) + (frac(diff v_x, diff z) - frac(diff v_z, diff x)) vuh(y) + (frac(diff v_y, diff x) - frac(diff v_x, diff y)) vuh(z) $

$nabla^2 t = frac(diff^2 t, diff x^2) + frac(diff^2 t, diff y^2) + frac(diff^2 t, diff z^2) $

=== Spherical
$nabla t = frac(diff t, diff r) vuh(r) + 1 / r frac(diff t, diff theta) vuh(theta) + frac(1, r sin theta) frac(diff t, diff phi.alt) vuh(phi.alt) $

$nabla dot.op vuc(v) = 1 / r^2 frac(diff, diff r) (r^2 v_r) + frac(1, r sin theta) frac(diff, diff theta) (sin theta v_theta) + frac(1, r sin theta) frac(diff v_phi.alt, diff phi.alt) $

$nabla times vuc(v) = frac(1, r sin theta) [frac(diff, diff theta) (sin theta v_phi.alt) - frac(diff v_theta, diff phi.alt)] vuh(r) + [frac(1, r sin theta) frac(diff v_r, diff phi.alt) - 1 / r frac(diff, diff r) (r v_phi.alt)] vuh(theta) + 1 / r [frac(diff, diff r) (r v_theta) - frac(diff v_r, diff theta)] vuh(phi.alt) $

$nabla^2 t = 1 / r^2 frac(diff, diff r) (r^2 frac(diff t, diff r)) + frac(1, r^2 sin theta) frac(diff, diff theta) (sin theta frac(diff t, diff theta)) + frac(1, r^2 sin^2 theta) frac(diff^2 t, diff phi.alt^2) $

=== Cylindrical

$nabla t = frac(diff t, diff s) vuh(s) + 1 / s frac(diff t, diff phi.alt) vuh(phi.alt) + frac(diff t, diff z) vuh(z) $

$nabla dot.op vuc(v) = 1 / s frac(diff, diff s) (s v_s) + 1 / s frac(diff v_phi.alt, diff phi.alt) + frac(diff v_z, diff z) $

$nabla times vuc(v) = (1 / s frac(diff v_z, diff phi.alt) - frac(diff v_phi.alt, diff z)) vuh(s) + (frac(diff v_s, diff z) - frac(diff v_z, diff s)) vuh(phi.alt) + 1 / s (frac(diff, diff s) (s v_phi.alt) - frac(diff v_s, diff phi.alt)) vuh(z) $

$nabla^2 t = 1 / s frac(diff, diff s) (s frac(diff t, diff s)) + 1 / s^2 frac(diff^2 t, diff phi.alt^2) + frac(diff^2 t, diff z^2) $


=== 2A
$integral(gradient dot vu(A)) dot d vu(a) = integral.cont vu(A) dot d vu(a)$

$integral(gradient times vu(A)) dot d vu(a) = integral.cont vu(A) dot d vu(l)$

]
