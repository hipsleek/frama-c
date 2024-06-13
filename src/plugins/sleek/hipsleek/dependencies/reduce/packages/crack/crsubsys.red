Wed Jul 16 11:06:27 MET DST 2003
REDUCE Development Version, 20-Jun-2003 ...

1: 
2: 2: 2: 2: 2: 2: 2: 2: 2: 
3: 3: load crack,applysym$


*** airy_ai already defined as operator 

*** airy_bi already defined as operator 

*** odesolve-specfn* already defined as operator 

*** odesolve-solns* already defined as operator 

%*******************************************************************%
%                                                                   %
%                    A P P L Y S Y M . T S T                        %
%                    -----------------------                        %
%  applysym.tst contains test examples to test the procedure        %
%  quasilinpde in the file applysym.red.                            %
%                                                                   %
%  Author: Thomas Wolf                                              %
%  Date:   22 May 1998                                              %
%                                                                   %
%  You need crack.red and applysym.red to run this demo.            %
%  To use other contents of the program applysym, not demonstrated  %
%  in this demo you need the program liepde.red.                    %
%                                                                   %
%  To run this demo you read in files with                          %
%     in "crack.red"$                                               %
%     in "applysym.red"$                                            %
%  or, to speed up the calculation you compile them before with     %
%     faslout "crack"$                                              %
%     in "crack.red"$                                               %
%     faslend$                                                      %
%     faslout "applysym"$                                           %
%     in "applysym.red"$                                            %
%     faslend$                                                      %
%  and then load them with                                          %
%     load crack,applysym$                                          %
%                                                                   %
%*******************************************************************%

load crack;



lisp(depl!*:=nil)$

     % clearing of all dependencies
setcrackflags()$


lisp(print_:=nil)$


on dfprint$



comment
-------------------------------------------------------
This file is supposed to provide an automatic test of
the program APPLYSYM. On the other hand the application
of APPLYSYM is an interactive process, therefore the
interested user should inspect the example described
in APPLYSYM.TEX which demonstrates the application
of symmetries to integrate a 2nd order ODE.
Here the program QUASILINPDE for integrating first
order quasilinear PDE is demonstrated.
The following equation comes up in the elimination
of resonant terms in normal forms of singularities
of vector fields (C.Herssens, P.Bonckaert, Limburgs
Universitair Centrum/Belgium, private communication);

write"-------------------"$


-------------------

lisp(print_:=nil)$



depend w,x,y,z$


QUASILINPDE( df(w,x)*x+df(w,y)*y+2*df(w,z)*z-2*w-x*y,  w,  {x,y,z} )$


   x*y
{{-----,
    z

    - log(z)*x*y + 2*w
  ---------------------,
            z

      x
  ---------}}
   sqrt(z)

nodepend w,x,y,z$



comment
-------------------------------------------------------
The result means that w is defined implicitly through 

        x*y    - log(z)*x*y + 2*w      y              
0 = ff(-----,---------------------,---------)         
         z             z            sqrt(z)           

with an arbitrary function ff of 3 arguments. As the PDE
was linear, the arguments of ff are such that we can 
solve for w:                                   

                        x*y      y             
w = log(z)*x*y/2 + z*f(-----,---------)        
                         z    sqrt(z)          

with an arbitrary function f of 2 arguments.
-------------------------------------------------------
The following PDEs are taken from E. Kamke,
Loesungsmethoden und Loesungen von Differential-
gleichungen, Partielle Differentialgleichungen
erster Ordnung, B.G. Teubner, Stuttgart (1979);


write"-------------------"$


-------------------
% equation 1.4 ----------------------
lisp(depl!*:=nil)$


depend z,x,y$


QUASILINPDE( x*df(z,x)-y, z, {x,y})$


{{log(x)*y - z,y}}

write"-------------------"$


-------------------
% equation 2.5 ----------------------
lisp(depl!*:=nil)$


depend z,x,y$


QUASILINPDE( x**2*df(z,x)+y**2*df(z,y), z, {x,y})$


    - x + y
{{----------,z}}
     x*y

write"-------------------"$


-------------------
% equation 2.6 ----------------------
lisp(depl!*:=nil)$


depend z,x,y$


QUASILINPDE( (x**2-y**2)*df(z,x)+2*x*y*df(z,y), z, {x,y})$


          2    2
      - (x  + y )
{{z,--------------}}
          y

write"-------------------"$


-------------------
% equation 2.7 ----------------------
lisp(depl!*:=nil)$


depend z,x,y$


QUASILINPDE( (a0*x-a1)*df(z,x)+(a0*y-a2)*df(z,y), z, {x,y})$


    a1*y - a2*x
{{----------------,z}}
   a1*(a0*x - a1)

write"-------------------"$


-------------------
% equation 2.14 ---------------------
lisp(depl!*:=nil)$


depend z,x,y$


QUASILINPDE( a*df(z,x)+b*df(z,y)-x**2+y**2, z, {x,y})$


   2  3            2      3        2  2      2  3
{{a *y  - 3*a*b*x*y  - 3*b *z + 3*b *x *y - b *y ,

  a*y - b*x}}

write"-------------------"$


-------------------
% equation 2.16 ---------------------
lisp(depl!*:=nil)$


depend z,x,y$


QUASILINPDE( x*df(z,x)+y*df(z,y)-a*x, z, {x,y})$


            - a*x
{{a*x - z,--------}}
             y

write"-------------------"$


-------------------
% equation 2.20 ---------------------
lisp(depl!*:=nil)$


depend z,x,y$


QUASILINPDE( df(z,x)+df(z,y)-a*z, z, {x,y})$


   a*x
{{e   ,x - y}}

write"-------------------"$


-------------------
% equation 2.21 ---------------------
lisp(depl!*:=nil)$


depend z,x,y$


QUASILINPDE( df(z,x)-y*df(z,y)+z, z, {x,y})$


   x    x
{{e *z,e *y}}

write"-------------------"$


-------------------
% equation 2.22 ---------------------
lisp(depl!*:=nil)$


depend z,x,y$


QUASILINPDE( 2*df(z,x)-y*df(z,y)+z, z, {x,y})$


   x/2    x/2
{{e   *z,e   *y}}

write"-------------------"$


-------------------
% equation 2.23 ---------------------
lisp(depl!*:=nil)$


depend z,x,y$


QUASILINPDE( a*df(z,x)+y*df(z,y)-b*z, z, {x,y})$


   (b*x)/a   y
{{e       ,------}}
             x/a
            e

write"-------------------"$


-------------------
% equation 2.24 ---------------------
lisp(depl!*:=nil)$


depend z,x,y$


QUASILINPDE( x*(df(z,x)-df(z,y))-y*df(z,y), z,{x,y})$


{{x*(x + 2*y),z}}

write"-------------------"$


-------------------
% equation 2.25 ---------------------
lisp(depl!*:=nil)$


depend z,x,y$


QUASILINPDE( x*df(z,x)+y*df(z,y)-az, z, {x,y})$


{{y,x}}

write"-------------------"$


-------------------
% equation 2.26 ---------------------
lisp(depl!*:=nil)$


depend z,x,y$


QUASILINPDE( x*df(z,x)+y*df(z,y)-z+x**2+y**2-1, z, {x,y})$


    2    2
   x  + y  + z + 1   x
{{-----------------,---}}
          y          y

write"-------------------"$


-------------------
% equation 2.39 ---------------------
lisp(depl!*:=nil)$


depend z,x,y$


QUASILINPDE( a*x**2*df(z,x)+b*y**2*df(z,y)-c*z**2, z, {x,y})$


   b*y - c*z
{{-----------,
     c*y*z

    - a*x + b*y
  --------------}}
      a*x*y

write"-------------------"$


-------------------
% equation 2.40 ---------------------
lisp(depl!*:=nil)$


depend z,x,y$


QUASILINPDE( x*y**2*df(z,x)+2*y**3*df(z,y)-2*(y*z-x**2)**2, z,
             {x,y})$


           4           2        2
   log(y)*x  - log(y)*x *y*z - y *z
{{----------------------------------,
             2   2
            x *(x  - y*z)

      x
  ---------}}
   sqrt(y)

write"-------------------"$


-------------------
% equation 3.12 ---------------------
lisp(depl!*:=nil)$


depend w,x,y,z$


QUASILINPDE( x*df(w,x)+(a*x+b*y)*df(w,y)+(c*x+d*y+f*z)*df(w,z), w,
             {x,y,z})$


                                                           2
    - a*d*x + b*c*x + b*f*z - b*z - c*f*x - d*f*y + d*y - f *z + f*z
{{-------------------------------------------------------------------,
                          f             2
                         x *(b*f - b - f  + f)

     d*(a*x + b*y - y)
  -----------------------,
    b   2
   x *(b  - b*f - b + f)

  w}}

write"-------------------"$


-------------------
% end -------------------------------

lisp(depl!*:=nil)$


end$

4: 4: 4: 4: 4: 4: 4: 4: 4: 

Time for test: 24440 ms, plus GC time: 830 ms

5: 5: 
Quitting
Wed Jul 16 11:08:27 MET DST 2003
cp,l1,l2,l1ul2,l1ml2,l2ml1,l1il2,oldorder,
      de1p,de2p,termsof1,termsof2,flip,n1,n2,ql,maxcancel,
      take_first,non_linear,homo,de2pnew,tr_short_local;
 non_linear:=t;
 % take_first:=t;
 % "=t is not so radical, --> eqn.s are longer and in total it is slower

 % tr_short_local:=t;
 if tr_short_local then deprint list(get(de1,'val),get(de2,'val))$

 if homogen_ and (1=car get(de1,'hom_deg)      )
             and (1=car get(de2,'hom_deg)      )
             and ((cadr get(de1,'hom_deg)) neq
                  (cadr get(de2,'hom_deg))     ) then homo:=t;
 if non_linear and null homo then <<
  a:=sort_partition(de1,nil,get(de1,'fcts),nil)$
  b:=sort_partition(de2,nil,get(de2,'fcts),nil)$
  if tr_short_local then <<
   write"a=",a$ terpri()$
   write"b=",b$ terpri()$
  >>;
  de1p:=nil;
  de2p:=nil;
  for each h in a do <<
   s:=car h;
   cp:=b;
   % Does s occur in b?
   while cp and (s neq caar cp) do cp:=cdr cp;
   if cp then <<
    r:=if (pairp s) or (numberp s) then gensym() else s;
    %--- dropping the ftem-depending factors once at the beginning
    de1p:=cons(cons(cadr h,
                    cons(r,
                         reval list('QUOTIENT,
                                    if cadr h>1 then cons('PLUS,caddr h)
                                                else caaddr h,
                                    s)
                        )),
               de1p);
    de2p:=cons(cons(cadar cp,
                    cons(r,
                         reval list('QUOTIENT,
                                    if cadar cp>1 then cons('PLUS,caddar cp)
                                                  else car caddar cp,
                                    s)
                        )),
               de2p);
%    %--- not dropping the ftem-depending factors
%    de1p:=cons(cons(cadr h,cons(r,if cadr h>1 then cons('PLUS,caddr h)
%                                              else caaddr h )),de1p);
%    de2p:=cons(cons(cadar cp,cons(r,if cadar cp>1 then cons('PLUS,caddar cp)
%                                                  else car caddar cp )),de2p);
    if tr_short_local then <<
     write"de1p=",de1p$terpri()$
     write"de2p=",de2p$terpri()$
    >>
   >>
  >>
 >>   else <<

  de1p:=get(de1,'val)$
  de2p:=get(de2,'val)$

  if homo then <<  % multiplication with flin_ functions is forbidden
   a:=get(de1,'derivs)$
   h:=nil$
   while a do <<
    if not freeoflist(car a,flin_) then h:=cons(car a,h);
    a:=cdr a
   >>
  >>      else h:=get(de1,'derivs)$
  l1:=for each a in h collect
      if length car a = 1 then caar a else cons('DF,car a)$ % all derivs of de1

  if homo then <<  % multiplication with flin_ functions is forbidden
   a:=get(de2,'derivs)$
   h:=nil$
   while a do <<
    if not freeoflist(car a,flin_) then h:=cons(car a,h);
    a:=cdr a
   >>
  >>      else h:=get(de2,'derivs)$
  l2:=for each a in h collect
      if length car a = 1 then caar a else cons('DF,car a)$ % all derivs of de2

  l1ml2:=setdiff(l1,l2);    % l1 - l2
  l2ml1:=setdiff(l2,l1);    % l2 - l1
  l1il2:=setdiff(l1,l1ml2); % intersection
  l1ul2:=union(l1,l2);      % union
  if tr_short_local then <<
   write"before substitution:"$terpri()$
   write"l1=",l1$ terpri()$
   write"l2=",l2$ terpri()$
   write"de1p=",de1p$terpri()$
   write"de2p=",de2p$terpri()$
   write"l1ml2=",l1ml2$terpri()$
   write"l2ml1=",l2ml1$terpri()$
   write"l1il2=",l1il2$terpri()$
   write"l1ul2=",l1ul2$terpri()$
  >>;

  % substituting derivatives by a new variable to become kernels
  for each a in l1ml2 do if pairp a then <<
   b:=gensym()$
   l1:=subst(b,a,l1)$
   l1ul2:=subst(b,a,l1ul2)$
   de1p:=subst(b,a,de1p)
  >>$
  for each a in l2ml1 do if pairp a then <<
   b:=gensym()$
   l2:=subst(b,a,l2)$
   l1ul2:=subst(b,a,l1ul2)$
   de2p:=subst(b,a,de2p)
  >>$
  for each a in l1il2 do if pairp a then <<
   b:=gensym()$
   l1:=subst(b,a,l1)$
   l2:=subst(b,a,l2)$
   l1ul2:=subst(b,a,l1ul2)$
   de1p:=subst(b,a,de1p)$
   de2p:=subst(b,a,de2p)
  >>$
  if tr_short_local then <<
   write"after substitution:"$terpri()$
   write"l1=",l1$ terpri()$
   write"l2=",l2$ terpri()$
   write"de1p=",de1p$terpri()$
   write"de2p=",de2p$terpri()$
   write"l1ml2=",l1ml2$terpri()$
   write"l2ml1=",l2ml1$terpri()$
   write"l1il2=",l1il2$terpri()$
   write"l1ul2=",l1ul2$terpri()$
  >>;

  %--- writing both equations as polynomials in elements of l1ul2
  oldorder:=setkorder l1ul2;
  de1p:=partition_1(de1p,l1); l1:=cdr de1p; de1p:=car de1p;
  de2p:=partition_1(de2p,l2); l2:=cdr de2p; de2p:=car de2p;
  setkorder oldorder;

  %--- l1,l2 can now have the element 1 in case of inhomogeneous de's
  l1ul2:=nil;
  l1il2:=nil;

  %--- Partitioning each equation into 2 parts, one part that can
  %--- be matched by the other equation and one that can not.

  % de1p:=partition_2(de1p,l2)$ dropped1:=car de1p; de1p:=cdr de1p;
  % de2p:=partition_2(de2p,l1)$ dropped2:=car de2p; de2p:=cdr de2p;
  de1p:=cdr partition_2(de1p,l2)$
  de2p:=cdr partition_2(de2p,l1)$
 >>$

 if (null de1p) or (null de2p) then return nil;

 termsof1:=no_of_terms get(de1,'val)$
 termsof2:=no_of_terms get(de2,'val)$

 if tr_short_local then <<
  write"---------"$terpri()$
  write"de1:",de1," with ",termsof1," terms"$terpri()$
  a:=de1p;
  while a do <<
   write "caar =",caar a;terpri()$
   write "cadar=",cadar a;terpri()$
   write "cddar=", algebraic write lisp cddar a;terpri()$
   a:=cdr a;
  >>;terpri()$
  write"de2:",de2," with ",termsof2," terms"$terpri()$
  a:=de2p;
  while a do <<
   write "caar =",caar a;terpri()$
   write "cadar=",cadar a;terpri()$
   write "cddar=",algebraic write lisp cddar a;terpri()$
   a:=cdr a;
  >>;terpri()$
 >>;

 % One can do a stronger restriction: The maximum that can be
 % canceled is sum of min of terms of the de1p,de2p sublists
 % corresponding to the coefficients of different ftem functions/deriv.

 a:=de1p; b:=de2p; n2:=nil;
 while a do <<
  n1:=if (caar a)<(caar b) then caar a else caar b;
  % n1 is min of terms of the coefficients of the same ftem function/der.
  n2:=cons(2*n1,n2);
  a:=cdr a; b:=cdr b;
 >>$

 % maxcancel is the maximal number of cancellations in all the
 % remaining runs of short depending on the current run.

 maxcancel:=list(0);
 n1:=0;
 while n2 do <<
  n1:=n1+car n2;
  n2:=cdr n2;
  maxcancel:=cons(n1,maxcancel);
 >>;

 if ((car maxcancel)<termsof1) and
    ((car maxcancel)<termsof2) then return nil;

 if homo and (cadr get(de1,'hom_deg)<cadr get(de2,'hom_deg)) then flip:=nil else
 if homo and (cadr get(de1,'hom_deg)>cadr get(de2,'hom_deg)) then flip:= t  else
 if (termsof1<termsof2) or
    (struc_eqn and
     (n1=n2)   and
     (null is_algebraic(de2))
    ) then flip:=nil
      else flip:=t;

 if flip then <<
  a:=de1p; de1p:=de2p; de2p:=a;
  n1:=termsof2;
  n2:=termsof1
 >>      else <<
  n1:=termsof1;
  n2:=termsof2
 >>;

 if (n1=1) and (length de1p = 1)
    and ((atom  cddar de1p) or (caddar de1p neq 'PLUS)) then <<
  % one equation has only a single term which is not a product of sums
  a:=cadar de1p;    % e.g. g0030
  b:=de2p;
  while b and (cadar b neq a) do b:=cdr b;
  if tr_short_local then <<
   write"one is a 1-term equation"$terpri()$
   write"a=",a$terpri()$
   write"b=",b$terpri()$
   write"de1p.1=",de1p$terpri()$
   write"de2p.1=",de2p$terpri()$
  >>$
  a:=if null b then nil  % that term does not turn up in other equation
               else <<   % it does turn up --> success
    de1p:=cddar de1p;
    de2p:=cddar b;
    if tr_short_local then <<
     write"de1p.2=",de1p$terpri()$
     write"de2p.2=",de2p$terpri()$
    >>$
    if homo then <<
     if pairp de2p and car de2p='PLUS then de2p:= cdr de2p
                                      else de2p:=liWed Jul 16 11:06:27 MET DST 2003
REDUCE Development Version, 20-Jun-2003 ...

1: 
2: 2: 2: 2: 2: 2: 2: 2: 2: 
3: 3: load crack,applysym$


*** airy_ai already defined as operator 

*** airy_bi already defined as operator 

*** odesolve-specfn* already defined as operator 

*** odesolve-solns* already defined as operator 

%*******************************************************************%
%                                                                   %
%                    A P P L Y S Y M . T S T                        %
%                    -----------------------                        %
%  applysym.tst contains test examples to test the procedure        %
%  quasilinpde in the file applysym.red.                            %
%                                                                   %
%  Author: Thomas Wolf                                              %
%  Date:   22 May 1998                                              %
%                                                                   %
%  You need crack.red and applysym.red to run this demo.            %
%  To use other contents of the program applysym, not demonstrated  %
%  in this demo you need the pr