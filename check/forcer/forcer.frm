* Requires the Forcer library: https://github.com/benruijl/forcer.
#ifndef `TEST'
  #message Use -D TEST=XXX
  #terminate
#else
  #include `NAME_' # `TEST'
#endif
.end

*--#[ Forcer_example :
#include- forcer.h
L F =
  + 1/<p1.p1>/.../<p6.p6>*
    Q.p3*Q.p4*vx(Q,p1,p5,p6)*vx(-p1,p2,p3)*vx(-p5,-p6,p4)*vx(-Q,-p2,-p3,-p4)
  + 1/<p1.p1>/.../<p5.p5>*
    vx(-Q,p2,p3)*vx(p1,-p2,p5)*vx(-p1,p4,Q)*vx(-p3,-p4,-p5)*ex(p1,p4)
;
#call Forcer(msbarexpand=4)
B ep;
P;
.end
assert succeeded?
assert result("F") =~ expr("
       + ep^-3 * ( 1/24 )

       + ep^-2 * ( 25/72 )

       + ep^-1 * ( 433/216 )

       + ep * ( 89089/1944 - 57/32*z4 - 725/72*z3 )

       + 6457/648 + 115/24*z3
")
*--#] Forcer_example : 
*--#[ Forcer_1 :
* timeout = 60 seconds.
#include- forcer.h
CF f,f1,f2,f3;
V p2,p3;
S x3;

* Give 1 or -1. n1 is not used.
Table randomsign(n1?);
Fill randomsign() = random_(2)*2-3;

* Zip two functions as:
*   zip(f1,f2(p1,...,pN),f3(q1,...,qN)) -> f1(p1,q1,...,pN,qN),
* for N >= 1.
Table zip(f1?(?a1),f2?(p2?,?a2),f3?(p3?,?a3));
Fill zip() =
  + thetap_(nargs_(?a2,?a3)) * zip(f1(?a1,p2,p3),f2(?a2),f3(?a3))
  + delta_(nargs_(?a2,?a3))  * f1(?a1,p2,p3)
;

* Element-wise multiplication as:
*   emul(f1,f2(p1,...,pN),f3(a1,...,aN)) -> f1(p1*a1,...,pN*aN)
* for N >= 1.
Table emul(f1?(?a1),f2?(p2?,?a2),f3?(x3?,?a3));
Fill emul() =
  + thetap_(nargs_(?a2,?a3)) * emul(f1(?a1,p2*x3),f2(?a2),f3(?a3))
  + delta_(nargs_(?a2,?a3))  * f1(?a1,p2*x3)
;

L F1 =
  #do i=1,3
    + Zno`i'(1,1,1,1,1,1,1,1,1,1,1,0,0,0)
  #enddo
;

L F2 =
  #do i=1,3
    + Zno`i'(2,1,1,1,1,1,1,1,1,1,1,0,0,0)
    + Zno`i'(1,2,1,1,1,1,1,1,1,1,1,0,0,0)
    + Zno`i'(1,1,2,1,1,1,1,1,1,1,1,0,0,0)
    + Zno`i'(1,1,1,2,1,1,1,1,1,1,1,0,0,0)
    + Zno`i'(1,1,1,1,2,1,1,1,1,1,1,0,0,0)
    + Zno`i'(1,1,1,1,1,2,1,1,1,1,1,0,0,0)
    + Zno`i'(1,1,1,1,1,1,2,1,1,1,1,0,0,0)
    + Zno`i'(1,1,1,1,1,1,1,2,1,1,1,0,0,0)
    + Zno`i'(1,1,1,1,1,1,1,1,2,1,1,0,0,0)
    + Zno`i'(1,1,1,1,1,1,1,1,1,2,1,0,0,0)
    + Zno`i'(1,1,1,1,1,1,1,1,1,1,2,0,0,0)
    + Zno`i'(1,1,1,1,1,1,1,1,1,1,1,-1,0,0)
    + Zno`i'(1,1,1,1,1,1,1,1,1,1,1,0,-1,0)
    + Zno`i'(1,1,1,1,1,1,1,1,1,1,1,0,0,-1)
  #enddo
;

id Zno1(n1?,...,n14?) =
  +vx(-Q,p4,p5)
  *vx(p3,-p4,p10)
  *vx(p2,-p3,p9)
  *vx(p1,-p2,p11)
  *vx(-p5,p6,-p11)
  *vx(-p6,p7,-p10)
  *vx(-p7,p8,-p9)
  *vx(-p1,-p8,Q)
  /<p1.p1^n1>/.../<p11.p11^n11>
  /p2.p4^n12/Q.p2^n13/Q.p3^n14
;

id Zno2(n1?,...,n14?) =
  +vx(-Q,p4,p5)
  *vx(p3,-p4,p11)
  *vx(p6,p7,p10)
  *vx(p2,-p3,-p10)
  *vx(p1,-p2,p9)
  *vx(-p5,-p6,-p9)
  *vx(-p7,p8,-p11)
  *vx(-p1,-p8,Q)
  /<p1.p1^n1>/.../<p11.p11^n11>
  /Q.p2^n12/p1.p4^n13/Q.p3^n14
;

id Zno3(n1?,...,n14?) =
  +vx(-Q,p3,p4)
  *vx(p6,p8,p10)
  *vx(p5,-p10,p11)
  *vx(p1,-p3,-p5)
  *vx(-p4,-p8,p9)
  *vx(p7,-p9,-p11)
  *vx(p2,-p6,-p7)
  *vx(-p1,-p2,Q)
  /<p1.p1^n1>/.../<p11.p11^n11>
  /Q.p6^n12/Q.p8^n13/p3.p6^n14
;

* Make a random permutation of the loop momenta. The result should be the same.
multiply f1(p1,...,p11);
multiply ranperm_(f2,p1,...,p11);
multiply f3(<randomsign(1)>,...,<randomsign(11)>);
id f2(?a)*f3(?b) = emul(f2,f2(?a),f3(?b));
id f1(?a)*f2(?b) = zip(f1,f1(?a),f2(?b));
id f1(?a) = replace_(?a);

ModuleOption noparallel;
.sort:input;

#call Forcer(msbarexpand=4)
B ep;
P;
.end
assert succeeded?
assert result("F1") =~ expr("
       + ep^-1 * (  - 35/2*z5 )

       + 21/2*z7 - 175/4*z6 + 105/4*z5 - 95/2*z3^2
")
assert result("F2") =~ expr("
       + ep^-4 * ( 15/2 )

       + ep^-3 * ( 383/12 )

       + ep^-2 * (  - 2089/18 )

       + ep^-1 * ( 1466/9 - 1245/8*z5 - 728*z3 )

       - 2183/12 + 441/2*z7 - 6225/16*z6 - 57155/48*z5 - 2169/2*z4 - 17198/9*
         z3 - 3705/8*z3^2
")
*--#] Forcer_1 : 
*--#[ Forcer_1-expand :
* timeout = 60 seconds.
#include- forcer.h
CF f,f1,f2,f3;
V p2,p3;
S x3;

* Give 1 or -1. n1 is not used.
Table randomsign(n1?);
Fill randomsign() = random_(2)*2-3;

* Zip two functions as:
*   zip(f1,f2(p1,...,pN),f3(q1,...,qN)) -> f1(p1,q1,...,pN,qN),
* for N >= 1.
Table zip(f1?(?a1),f2?(p2?,?a2),f3?(p3?,?a3));
Fill zip() =
  + thetap_(nargs_(?a2,?a3)) * zip(f1(?a1,p2,p3),f2(?a2),f3(?a3))
  + delta_(nargs_(?a2,?a3))  * f1(?a1,p2,p3)
;

* Element-wise multiplication as:
*   emul(f1,f2(p1,...,pN),f3(a1,...,aN)) -> f1(p1*a1,...,pN*aN)
* for N >= 1.
Table emul(f1?(?a1),f2?(p2?,?a2),f3?(x3?,?a3));
Fill emul() =
  + thetap_(nargs_(?a2,?a3)) * emul(f1(?a1,p2*x3),f2(?a2),f3(?a3))
  + delta_(nargs_(?a2,?a3))  * f1(?a1,p2*x3)
;

L F1 =
  #do i=1,3
    + Zno`i'(1,1,1,1,1,1,1,1,1,1,1,0,0,0)
  #enddo
;

L F2 =
  #do i=1,3
    + Zno`i'(2,1,1,1,1,1,1,1,1,1,1,0,0,0)
    + Zno`i'(1,2,1,1,1,1,1,1,1,1,1,0,0,0)
    + Zno`i'(1,1,2,1,1,1,1,1,1,1,1,0,0,0)
    + Zno`i'(1,1,1,2,1,1,1,1,1,1,1,0,0,0)
    + Zno`i'(1,1,1,1,2,1,1,1,1,1,1,0,0,0)
    + Zno`i'(1,1,1,1,1,2,1,1,1,1,1,0,0,0)
    + Zno`i'(1,1,1,1,1,1,2,1,1,1,1,0,0,0)
    + Zno`i'(1,1,1,1,1,1,1,2,1,1,1,0,0,0)
    + Zno`i'(1,1,1,1,1,1,1,1,2,1,1,0,0,0)
    + Zno`i'(1,1,1,1,1,1,1,1,1,2,1,0,0,0)
    + Zno`i'(1,1,1,1,1,1,1,1,1,1,2,0,0,0)
    + Zno`i'(1,1,1,1,1,1,1,1,1,1,1,-1,0,0)
    + Zno`i'(1,1,1,1,1,1,1,1,1,1,1,0,-1,0)
    + Zno`i'(1,1,1,1,1,1,1,1,1,1,1,0,0,-1)
  #enddo
;

id Zno1(n1?,...,n14?) =
  +vx(-Q,p4,p5)
  *vx(p3,-p4,p10)
  *vx(p2,-p3,p9)
  *vx(p1,-p2,p11)
  *vx(-p5,p6,-p11)
  *vx(-p6,p7,-p10)
  *vx(-p7,p8,-p9)
  *vx(-p1,-p8,Q)
  /<p1.p1^n1>/.../<p11.p11^n11>
  /p2.p4^n12/Q.p2^n13/Q.p3^n14
;

id Zno2(n1?,...,n14?) =
  +vx(-Q,p4,p5)
  *vx(p3,-p4,p11)
  *vx(p6,p7,p10)
  *vx(p2,-p3,-p10)
  *vx(p1,-p2,p9)
  *vx(-p5,-p6,-p9)
  *vx(-p7,p8,-p11)
  *vx(-p1,-p8,Q)
  /<p1.p1^n1>/.../<p11.p11^n11>
  /Q.p2^n12/p1.p4^n13/Q.p3^n14
;

id Zno3(n1?,...,n14?) =
  +vx(-Q,p3,p4)
  *vx(p6,p8,p10)
  *vx(p5,-p10,p11)
  *vx(p1,-p3,-p5)
  *vx(-p4,-p8,p9)
  *vx(p7,-p9,-p11)
  *vx(p2,-p6,-p7)
  *vx(-p1,-p2,Q)
  /<p1.p1^n1>/.../<p11.p11^n11>
  /Q.p6^n12/Q.p8^n13/p3.p6^n14
;

* Make a random permutation of the loop momenta. The result should be the same.
multiply f1(p1,...,p11);
multiply ranperm_(f2,p1,...,p11);
multiply f3(<randomsign(1)>,...,<randomsign(11)>);
id f2(?a)*f3(?b) = emul(f2,f2(?a),f3(?b));
id f1(?a)*f2(?b) = zip(f1,f1(?a),f2(?b));
id f1(?a) = replace_(?a);

ModuleOption noparallel;
.sort:input;

#call Forcer(msbarexpand=4,polyratfunexpand=15)
B ep;
P;
.end
assert succeeded?
assert result("F1") =~ expr("
       + ep^-1 * (  - 35/2*z5 )

       + 21/2*z7 - 175/4*z6 + 105/4*z5 - 95/2*z3^2
")
assert result("F2") =~ expr("
       + ep^-4 * ( 15/2 )

       + ep^-3 * ( 383/12 )

       + ep^-2 * (  - 2089/18 )

       + ep^-1 * ( 1466/9 - 1245/8*z5 - 728*z3 )

       - 2183/12 + 441/2*z7 - 6225/16*z6 - 57155/48*z5 - 2169/2*z4 - 17198/9*
         z3 - 3705/8*z3^2
")
*--#] Forcer_1-expand : 
