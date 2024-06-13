%*********************************************************************
module integration$
%*********************************************************************
%  Routines for integration of pde's
%  Authors: Andreas Brand 1993 1995
%           Thomas Wolf since 1993

symbolic procedure ldlist(p,f,vl)$
% provides a reverse list of leading derivatives of f in p, vl is list
% of variables
begin scalar a$
  if not atom p then
  if member(car p,list('EXPT,'PLUS,'MINUS,'TIMES,'QUOTIENT,'DF,'EQUAL))
  then <<
    if (car p='PLUS) or (car p='TIMES) or
       (car p='QUOTIENT) or (car p='EQUAL) then
    <<p:=cdr p$
      while p do
      <<a:=diffincl(a,ldlist(car p,f,vl))$
        p:=cdr p>>
    >>                                     else
    if car p='MINUS then a:=ldlist(cadr p,f,vl) else
    if car p='EXPT         then   % if numberp caddr p then
    a:=ldlist(cadr p,f,vl) else   % fuehrende Abl. aus der Basis
                                  % else a:=nil
    if car p='DF then if cadr p=f then
    <<p:=cddr p;
      while vl do
      <<a:=cons(dfdeg(p,car vl),a);
        vl:=cdr vl>>;
      a:=list a
    >>
  >>$
  return a
end$

symbolic procedure diffincl(a,b)$
% a,b are lists of leading derivatives which are to be united
begin
  scalar p;
  while a and b do
  <<a:=ddroplow(a,car b);
    if car a then p:=cons(car a,p);
    a:=cdr a;
    b:=cdr b>>;
  return
  if null a then if p then nconc(p,b)
                      else b
            else if p then a:=nconc(p,a)
                      else a
end$

symbolic procedure ddroplow(a,cb)$
% loescht Elemente von a, von denen cb eine Ableitung ist, loescht cb,
% wenn ein Element von a eine Ableitung von cb ist
begin
  scalar h;
  return
  if null a then list(cb)
            else
  <<h:=compdiffer(car a,cb);
    if numberp(h) then if h>0 then cons(nil,a)        % car a=df(cb,..
                              else ddroplow(cdr a,cb) % cb=df(car a,..
                  else <<h:=ddroplow(cdr a,cb);       % neither
                         cons(car h,cons(car a,cdr h))>>
  >>
end$

symbolic procedure compdiffer(p,q)$
% finds whether p is a derivative of q or q of p or neither
begin
  scalar p!>q,q!>p;
  while p and ((null p!>q) or (null q!>p)) do
  <<if car p>car q then p!>q:=t else  % compare orders of derivatives
    if car p<car q then q!>p:=t;
    p:=cdr p;q:=cdr q
  >>;
  return
  if q!>p then if p!>q then nil     %  neither
                       else 0       %  q is derivative of p
          else if p!>q then 2       %  p is derivative of q
                       else 1       %  p equal q
end$


symbolic procedure ldintersec(a)$
% determines the intersection of derivatives in the list a
begin
  scalar b;
  return
  if null a then nil else
  <<b:=car a;a:=cdr a;
    while a do
    <<b:=isec(b,car a)$
      a:=cdr a
    >>;
    b
  >>
end$


symbolic procedure isec(ca,b)$
% determines the minimum derivatives between ca and b
begin
  scalar newb;
  while ca do
  <<newb:=cons(if car b<car ca then car b else car ca, newb);
    ca:=cdr ca;b:=cdr b
  >>;
  return reverse newb
end$


symbolic procedure disjun(a,b)$
<<while a do
  if (car a neq 0) and (car b neq 0) then a:=nil
                                     else <<a:=cdr a;b:=cdr b>>;
  if b then nil else t
>>$


symbolic procedure ddrophigh(a,cb)$
% loescht Elemente von a, die Ableitung von cb sind,
% loescht cb, wenn cb Ableitung eines Elements von a ist oder =a ist,
% haengt cb ansonsten an
begin
  scalar h;
  return
  if null a then list(cb)
            else
  <<h:=compdiffer(car a,cb);
    if numberp(h) then if h<2 then a         % cb is deriv or = car a
                              else ddrophigh(cdr a,cb) % car a=df(cb,..
                  else cons(car a,ddrophigh(cdr a,cb)) % neither
  >>
end$


symbolic procedure elibar(l1,l2,lds)$
begin
  scalar found1,found2,h;
  % found1=t if an LD=l1 is found, found2=t if contradiction found
  while lds and (not found2) do
  <<if car lds=l1 then found1:=t else
    <<h:=compdiffer(l2,car lds);
      if (null h) or (h = 2) then found2:=t
    >>;
    lds:=cdr lds
  >>;
  return found1 and (not found2)
end$

symbolic procedure intminderiv(p,ftem,vlrev,maxvanz,nfsub)$
% yields a list {nv1,nv2, ... } such that nvi is the minimum
% of the highest derivatives w.r.t. vi of all the leading derivatives
% of all functions of ftem which are functions of all maxvanz variables.
% Only those are kept for which nvi>0.
% further a list ((f1 ld's of f1) (f2 ld's of f2)...),
begin scalar l,a,listoflds$
  while ftem do
  <<if (maxvanz=(fctlength car ftem)) or (nfsub=0) then
    <<l:=ldlist(p,car ftem,vlrev);
      listoflds:=cons(cons(car ftem,l),listoflds)$
      a:=if a then ldintersec(cons(a,l))
              else ldintersec(l)
    >>$
    ftem:=cdr ftem
  >>$
  return list(a,listoflds)
end$


symbolic procedure potintegrable(listoflds)$
begin
  scalar h,l1,l2,f,n,lds,f1,f2$
  if tr_genint then write "Does a potential exist?"$
  %------- Determining 2 minimal sets of integration variables
  % There must be two disjunct LDs such that all others are in their
  % ideal. This is necessary if we want to eliminate 2 functions.
  h:=listoflds;
  l1:=nil;
  while h do
  <<l2:=cdar h; % the list of LDs for the function caar h
    while l2 do
    <<l1:=ddrophigh(l1,car l2)$
      l2:=cdr l2>>;
    h:=cdr h
  >>;
  return
  if length l1 neq 2 then nil else
  if not disjun(car l1,cadr l1) then nil else
  % if there would be an overlap between l1 and l2 then it would have
  % to be integrated for elimination but it cannot --> no overlap
  % possible
  <<% selecting interesting functions for which one LD is = l1 and all
    % others are derivatives of l2 or equal l2 and vice versa. Two
    % necessary (one with an LD=l1 and one with an LD=l2) from which at
    % least one function (f) has no further LD.
    % Exception: 2 functions with each 2 LDs equal to (l1,l2) (these
    % functions are counted by n)
    l2:=cadr l1;l1:=car l1;
    f:=nil;f1:=nil;f2:=nil;n:=0;
    h:=listoflds;
    while h and ((not f1) or (not f2) or ((not f) and (n neq 2))) do
    <<lds:=cdar h;
      if (not f1) or (not f) then
      if elibar(l1,l2,lds) then
      <<f1:=cons(caar h,f1);
        if length lds = 1 then f:=caar h else
        if length lds = 2 then
        if (car lds = l2) or (cadr lds = l2) then n:=n+1
      >>;
      if (not f2) or (not f) then
      if elibar(l2,l1,lds) then
      <<f2:=cons(caar h,f2);
        if length lds = 1 then f:=caar h
      >>;
      h:=cdr h
    >>;
    if f1 and ((n>1) or (f2 and f)) then list(l1,l2)
                                    else nil
  >>
end$ % of potintegrable

symbolic procedure vlofintlist(vl,intlist)$
% provides a list of elements of vl for which the corresponding
% elements of intlist are non-zero
begin scalar a;
  while intlist do
  <<if (car intlist) and (not zerop car intlist) then a:=cons(car vl,a);
    vl:=cdr vl;
    intlist:=cdr intlist
  >>;
  return a
end$

symbolic procedure vlofintfaclist(vl,intfaclist)$
% determining the list of all variables of vl in intfaclist
begin scalar e1,a;
  for each e1 in vl do
  if not my_freeof(intfaclist,e1) then a:=cons(e1,a);
  return a
end$

symbolic procedure multipleint(intvar,ftem,q,vari,vl,genflag,
                               potflag,partial,doneintvar)$
% multiple integration of q wrt. variables in vl, max. number of
% integrations specified by intvar
% integrating factors must not depend on doneintvar, doneintvar is
% a list of integration variables so far
% partial=t then as much as possible of an expression is integrated
% even if there is a remainder
begin
  scalar pri,vlcop,v,nmax,geni,intlist,iflag,n,nges,newcond,
         intfaclist,ph,pih,qh,qih,intfacdepnew,intfacdep$
  % intfacdep is a list of variables on which factors of integration
  % depend so far, other than the integration variable in their
  % integration --> no integration wrt. these variables by potint
  % because there the diff. operators wrt. to different variables
  % need not commute because the integrations are not done

  % pri:=t$
  if (not vari) and (zerop q) then return nil;
  nges:=0;
  vlcop:=vl;
  pih:=t;

  % Splitting of the equation into the homogeneous and inhomogeneous
  % part which is of advantage for finding integrating factors
  q:=splitinhom(q,ftem,vl)$
  qh:=car q; qih:=cdr q; q:=nil;

  while (vari or vlcop) and (pih or (not potflag)) do
  %------- if for potflag=t one variable can not be integrated the
  %------- maximal number of times (nmax) then immediate stop because
  %------- then no elimination of two functions will be possible
  << %-- The next integration variable: v, no of integrations: nmax
    if vari then <<v:=vari;nmax:=1>>
            else <<v:=car vlcop;     vlcop:=cdr vlcop;
                   nmax:=car intvar; intvar:=cdr intvar>>;

    if zerop nmax then intlist:=cons(nil,intlist)
                  else
    <<if pri then write"anf: intvar=",intvar," vari=",vari,"    q=",q$
      if vari and (not member(v,vl)) then
      <<qh :=reval list('INT,qh ,v)$
        if freeof(qh,'INT) then <<
          qih:=reval list('INT,qih,v)$
          iflag:=if freeint_ and
                    (null freeof(qih,'INT)) then nil else
                 if freeabs_ and
                    (null freeof(qih,'ABS)) then nil else <<
                   intlist:=cons(list(1),intlist)$
                   'success>>$
          if pri then <<write"232323 qh=",qh;terpri();
                        write"qih=",qih;terpri()>>
        >>
      >>                             else
      <<n:=0$
        if pri then write"333"$
        intfaclist:=nil; %-- the list of integr. factors in v-integr.
        if potflag or my_freeof(intfacdep,v) then
        % otherwise v-integration not allowed because one integrating
        % factor already depends on v
        % for potflag=t this `commutativity'-demand plays no role
        repeat << %--- max nmax integrations of qh and qih wrt. v
          if pri then <<write"444  vor intpde:"$eqprint q$terpri()$
                        write"potflag=",potflag," v=",v,
                        "  ftem=",ftem>>$
          % At first trying a direct integration of the homog. part qh
          ph:=intpde(qh,ftem,vl,v,partial)$  % faster if potflag=nil
          if pri then <<write"nach intpde(qh):"$deprint ph>>$

          %------ At first the integration of the homogeneous part
          intfacdepnew:=intfacdep;
          if ph and (partial or (zerop cadr ph)) then <<
            %---- For the homogen. part cadr ph must be zero
            intfaclist:=cons(1,intfaclist);
            ph:=car ph;
            if pri then <<write"565656 ph=",ph;terpri()>>;
          >>                                     else
          if vari then ph:=nil
                  else
          if facint_ then <<
            ph:=findintfac(list(qh),ftem,vl,v,doneintvar,intfacdep,
                           not zerop n,not potflag);
            % factorize before ivestig., no report of int. factors
            if ph then << %--- Complete integr. of qh was possible
              if pri then write"of the homogeneous part"$terpri()$
              %--- update the list of variables on which all integr.
              %--- factors depend apart from the integration variable
              intfacdepnew:=caddr ph;
              %--- extend the list of integrating factors, cadr ph
              %--- is a list of integr. factors, here only one
              intfaclist:=cons(caadr ph,intfaclist);
              %--- multiply the inhomogeneous part with integ. factor
              qih:=reval reval reval list('TIMES,car intfaclist,qih);
              if pri then <<write"454545 qih=",qih;terpri()>>;
              ph:=car ph  % the integral of qh
            >>
          >>;

          %------ Now the integration of the inhomogeneous part
          if not ph then pih:=nil %--- no integration possible
                    else <<
            if zerop qih then pih:=list(0,0) else
            pih:=intpde(qih,ftem,vl,v,partial)$

            if print_ and null pih then <<
              terpri()$
              write"Inhomogeneous part: "$
              mathprint qih$
              write"can not be integrated explicitly wrt. ",v$
            >>$
            if pri then <<write"nach intpde(qih):",pih$terpri()$
                          write"genflag=",genflag$terpri()>>$

            if pih then
            if zerop cadr pih then
            <<qih:=car pih$n:=add1 n$iflag:='success$
              if pri then write" success "$
            >>
                              else if not genflag then pih:=nil
                                                  else
            <<if pri then write"555"$
              geni:=partint(cadr pih,smemberl(ftem,cadr pih),
                            vl,v,genflag)$
              if geni then
              <<qih:=reval list('PLUS,car pih,car geni)$
                n:=add1 n$
                ftem:=union(fnew_,ftem)$
                newcond:=append(cdr geni,newcond)$  % additional de's
                if pri then
                <<terpri()$write"nach gen newcond:",newcond>>$
                iflag:='genint
              >>                       else
              if partial then qih:=car pih
                         else pih:=nil
            >>;
            if pih then <<
              if pri then write"AAA"$
               qh:=ph;
              if (not potflag) and (n neq 1) then
              intfacdep:=intfacdepnew
              %-The first integr. factor of all v-integrations does not
              % depend on any earlier integration variables and can
              % therefore be taken out of all integrals --> no incease
              % of intfacdep for n=1.
              %-For potential integration the integration variables and
              % extra-dependencies-variables of integr. factors need not
              % be disjunct therefore the intfacdep-update only for
              % not-potflag
            >>     else <<
              if pri then write"BBB"$
              % inhomogeneous part can not be integrated therefore
              % reversing the succesful integration of the hom. part
              if car intfaclist neq 1 then
              qih:=reval list('QUOTIENT,qih,car intfaclist);
              intfaclist:=cdr intfaclist
            >>;
          >>; %-- end of the integration of the inhomog. part
          if pri then write"n=",n," nmax=",nmax," not pih=",not pih$
        >> until (n=nmax) or (not pih)$ %---- end of v-integration
        %------- variables of done integrations are collected for
        %------- determining integrating factors in later integr.
        if not zerop n then doneintvar:=cons(v,doneintvar)$
        nges:=nges+n;
        intlist:=cons(intfaclist,intlist)
      >>$  % of    not ( vari and (not member(v,vl)))
      if vari then <<vari:=nil;vlcop:=nil>>;
      if pri then write"ende: intvar=",intvar," vari=",vari,
      "    qh=",qh,"   qih=",qih$
    >> % of (nmax neq zero)
  >>$  % of ( while (vari or vlcop) and (pih or (not potflag)) )

  % intlist and its elements intfaclist are in the inverse order
  % to vl and the sequence of integrations done
  q:=reval list('PLUS,qh,qih)$ %--- adding homog. and inhomog. part
  if pri then <<terpri()$write"\\\  newcond:"$deprint newcond;
    write"multiple result:",if null iflag then nil
    else list(q,intlist,newcond,nges)
  >>;
  return if null iflag then nil
                       else list(q,intlist,newcond,nges)
end$  % of multipleint

symbolic procedure uplistoflds(intlist,listoflds)$
begin
  scalar f,h1,h2,h3,h4,lds,itl;
  while listoflds do
  <<f:=caar listoflds;
    lds:=cdar listoflds;
    listoflds:=cdr listoflds;
    h2:=nil;            % h2 becomes the new list of lds of f
    while lds do
    <<h3:=car lds; lds:=cdr lds;
      itl:=intlist;
      h4:=nil;          % h4 becomes one new ld of f
      while h3 do
      <<h4:=cons(car h3 - if null car itl then 0
                                          else length car itl, h4);
        h3:=cdr h3;itl:=cdr itl
      >>;
      h2:=cons(reverse h4,h2)
    >>;
    h1:=cons(cons(f,h2),h1)
  >>;
  return h1  % updated listoflds
end$ % of uplistoflds

symbolic procedure addintco(q, ftem, ifac, vl, vari)$
begin scalar v,f,l,vl1;
  % multi.ing factors to the constants/functions of integration
  if zerop q then l:=1
             else
  <<ftem:=fctsort ftem$
    while ftem do
    if fctlength car ftem<length vl then ftem:=nil
                                    else if fctlinear(q,f)          then
                                         <<f:=car ftem$ ftem:=nil>> else
                                         ftem:=cdr ftem$
    if f then
    <<l:=lderiv(q,f,fctargs f)$
      l:=reval coeffn(q,reval car l,cdr l)
    >>   else l:=1
  >>;
  % the constants and functions of integration
  if vari then q:=list('PLUS,q,intconst(l,vl,vari,list(1)))
          else
  <<vl1:=vl;
    while vl1 do
    <<v:=car vl1; vl1:=cdr vl1;
      if car ifac then
      q:=list('PLUS,q,intconst(l,vl,v,car ifac))$
      % l..product of factors in the coefficient of the function to be
      % eliminated, car ifac .. list of integrating factors
      ifac:=cdr ifac;
    >>
  >>$
  return reval q
end$ % of addintco

symbolic procedure integratepde(p,ftem,vari,genflag,potflag)$
%  Generalized integration of the expression p
%     if not genflag then "normal integration"
%  Equation p must not be directly separable, otherwise the depen-
%  dencies of functions of integration on their variables is wrong,
%  i.e. no dependence on explicit variables
%  ftem are all functions from the `global' ftem which occur in p, i.e.
%  ftem:=smemberl(ftem,p)$
%  if vari=nil then integration w.r.t. all possible variables within
%                   the equation
%              else only w.r.t. vari one time

begin
  scalar vl,vlrev,v,intlist,
  ili1a,ili2a,maxvanz,fsub,h,hh,nfsub,iflag,newcond,
  n1,n2,pot1,pot2,p1,p2,listoflds,secnd,ifac0,
  ifac1a,ifac1b,ifac2a,ifac2b,cop,v1a,v2a,pri$

  % pri:=t;
  if pri then <<terpri()$write"Start Integratepde">>$
  vl:=argset ftem$
  vlrev:=reverse vl;
  if vari then <<potflag:=nil;
                 if zerop p then iflag:='success>>
          else
  <<%------- determining fsub=list of functions of all variables
    maxvanz:=length vl$
    fsub:=nil;
    h:=ftem;
    while h do
    <<if fctlength car h=maxvanz then
      fsub:=cons(car h,fsub)$
      h:=cdr h
    >>$
    nfsub:=length fsub$  % must be >1 for potential-integration
    h:=intminderiv(p,ftem,vlrev,maxvanz,nfsub)$ % fsub is also for below
    intlist:=car h$
    %-- list of necessary integrations of the whole equation to solve
    %-- for a function of all variables
    listoflds:=cadr h$ %-- list of leading derivatives
  >>$
  if pri then <<terpri()$
                write"complete integrations:",intlist," for:",vl>>;
  %-- n1 is the number of integrations which must be done to try
  %-- potential integration which must enable to eliminate 2 functions
  %-- n2 is the number of integrations actually done
  n1:=for each h in intlist sum h;
  if (not vari) and (zerop n1) then
  <<n2:=0;
    if potflag then % else not necessary
    for h:=1:(length vl) do ifac0:=cons(nil,ifac0)
  >>                           else
  <<if tr_genint then
    <<terpri()$write "integration of the expression : "$
      eqprint p>>$
    if pri then
    <<terpri()$write"at first all multiple complete integration">>;
    %-- At first if possible n2 integrations of the whole equation
    h:=multipleint(intlist,ftem,p,vari,vl,genflag,nil,nil,nil)$
                   % potflag=nil, partial=nil, doneintvar=nil
    if h then
    <<p:=car h;
      ifac0:=cadr h;  % ifac0 is the list of lists of integr. factors
      newcond:=caddr h;
      n2:=cadddr h;   % number of done integrations
      % doneintvar & intfacdep for the two halfs of integrations
      % from the two parts of ifac0
      h:=nil;
      iflag:='success;
    >>   else n2:=0;
    ftem:=union(fnew_,ftem)$
  >>;
  %------------ Existence of a potential ?
  if (n1=n2) and potflag and (nfsub>1) then
  %---- at least 2 functions to solve for
  <<if not zerop n2 then            %---- update listoflds
    listoflds:=uplistoflds(reverse ifac0,listoflds)$
    if pri then <<terpri()$write"uplistoflds:",listoflds>>$
    if h:=potintegrable(listoflds) then
    <<ili1a:=car h; ili2a:=cadr h;
      % The necess. differentiations of the potential
      if pri then
      <<terpri()$write"potintegrable:",ili1a,"  ",ili2a>>$

      if pri then <<write"+++ intlist=",intlist,
                           "    ili1a=",ili1a,
                           "    ili2a=",ili2a>>$
      %-- distributing the integrating factors of ifac0 among
      %-- the two lists ifac1b and ifac2b which are so far nil
      %-- such that (ifac1b and ili1a are disjunct) and
      %--           (ifac2b and ili2a are disjunct)
      v1a:=vlofintlist(vl,ili1a);
      v2a:=vlofintlist(vl,ili2a);

      hh:=t;
      cop:=reverse ifac0;
      ifac1a:=ili1a;ifac2a:=ili2a;
      while hh and cop do <<
        % cop is a list of lists of integr. factors
        if car cop then h:=vlofintfaclist(vl,cdar cop)
                   else h:=nil;
        if freeoflist(h,v2a) and (car ifac2a=0) then <<
          ifac1b:=cons( nil, ifac1b);
          ifac2b:=cons( reverse car cop, ifac2b)
        >>                   else
        if freeoflist(h,v1a) and (car ifac1a=0) then <<
          ifac2b:=cons( nil, ifac2b);
          ifac1b:=cons( reverse car cop, ifac1b)
        >>                   else
        if car cop then hh:=nil;
        ifac1a:=cdr ifac1a;
        ifac2a:=cdr ifac2a;
        cop:=cdr cop;
      >>;
      % the elements of ifac1b,ifac2b are in reverse order to
      % ifac1a,ifac2a and are in the same order as vl, also
      % the elements in the infac-lists are in inverse order,
      % i.e. in the order the integrations have been done
      if pri then <<terpri()$
                    write  "ifac1a=",ifac1a,"  ifac1b=",ifac1b,
                    "  ifac2a=",ifac2a,"  ifac2b=",ifac2b >>$

      %-- lists of integrations to be done to both parts
      if hh then
      repeat % possibly a second try with part2 integrated first
      <<n1:=for each n1 in ili1a sum n1;
        % n1 .. number of remaining integrations of the first half
        p1:=multipleint(ili1a,ftem,p,nil,vl,genflag,t,t,
                        % potflag=t, partial=t
                        union(vlofintlist(vl,ili2a),
                              vlofintlist(vl,ifac1b)))$
        % that the variables of integration are not in ifac1b
        % was already checked. Only restriction: the integrating
        % factors must not depend on the last argument.

        ftem:=union(fnew_,ftem)$
        if p1 then <<
          ifac1a:=cadr p1;
          % ifac1a is now the list of integrating factors
          if newcond then newcond:=nconc(newcond,caddr p1)
                     else newcond:=caddr p1;
          if pri then <<terpri()$write"mul2: newcond=",newcond>>$
          n2:=cadddr p1;
          p1:=car p1
        >>;
        if p1 and (n1=n2) then
        %--- if the first half has been integrated suff. often
        <<%--- integrating the second half sufficiently often
          n1:=for each n1 in ili2a sum n1;
          % calculation of the 2. part which is not contained in p1
          p2:=p1;
          cop:=ifac1a; hh:=vlrev; % because ifac1a is reversed
          while cop do <<
            h:=car cop;cop:=cdr cop;
            v:=car hh;hh:=cdr hh;
            % h is the list of integrating factors of the v-integr.
            while h do <<
              p2:=reval list('QUOTIENT,list('DF,p2,v),car h);
              h:=cdr h
            >>
          >>;
          p2:=reval reval list('PLUS,p,list('MINUS,p2));
          p2:=multipleint(ili2a,ftem,p2,nil,vl,genflag,t,nil,
                          % potflag=t, partial=nil
                          union(vlofintlist(vl,ili1a),
                                vlofintlist(vl,ifac2b)))$
          ftem:=union(fnew_,ftem)$
          if p2 then <<
            ifac2a:=cadr p2;
            % ifac2a is now list of integrating factors
            if newcond then newcond:=nconc(newcond,caddr p2)
                       else newcond:=caddr p2;
            if pri then <<terpri()$write"mul3: newcond=",newcond>>$
            n2:=cadddr p2;
            p2:=car p2
          >>;
          if p2 and (n1=n2) then
          % if the second half has been integrated sufficiently often
          <<% can both halfes be solved for different functions
            % i.e. are inhomogeneous
  % part which is of advantage for finding integrating factors
  q:=splitinhom(q,ftem,vl)$
  qh:=car q; qih:=cdr q; q:=nil;

  while (vari or vlcop) and (pih or (not potflag)) do
  %------- if for potflag=t one variable can not be integrated the
  %------- maximal number of times (nmax) then immediate stop because
  %------- then no elimination of two functions will be possible
  << %-- The next integration variable: v, no of integrations: nmax
    if vari then <<v:=vari;nmax:=1>>
            else <<v:=car vlcop;     vlcop:=cdr vlcop;
                   nmax:=car intvar; intvar:=cdr intvar>>;

    if zerop nmax then intlist:=cons(nil,intlist)
                  else
    <<if pri then write"anf: intvar=",intvar," vari=",vari,"    q=",q$
      if vari and (not member(v,vl)) then
      <<qh :=reval list('INT,qh ,v)$
        if freeof(qh,'INT) then <<
          qih:=reval list('INT,qih,v)$
          iflag:=if freeint_ and
                    (null freeof(qih,'INT)) then nil else
                 if freeabs_ and
                    (null freeof(qih,'ABS)) then nil else <<
                   intlist:=cons(list(1),intlist)$
                   'success>>$
          if pri then <<write"232323 qh=",qh;terpri();
                        write"qih=",qih;terpri()>>
        >>
      >>                             else
      <<n:=0$
        if pri then write"333"$
        intfaclist:=nil; %-- the list of integr. factors in v-integr.
        if potflag or my_freeof(intfacdep,v) then
        % otherwise v-integration not allowed because one integrating
        % factor already depends on v
        % for potflag=t this `commutativity'-demand plays no role
        repeat << %--- max nmax integrations of qh and qih wrt. v
          if pri then <<write"444  vor intpde:"$eqprint q$terpri()$
                        write"potflag=",potflag," v=",v,
                        "  ftem=",ftem>>$
          % At first trying a direct integration of the homog. part qh
          ph:=intpde(qh,ftem,vl,v,partial)$  % faster if potflag=nil
          if pri then <<write"nach intpde(qh):"$deprint ph>>$

          %------ At first the integration of the homogeneous part
          intfacdepnew:=intfacdep;
          if ph and (partial or (zerop cadr ph)) then <<
            %---- For the homogen. part cadr ph must be zero
            intfaclist:=cons(1,intfaclist);
            ph:=car ph;
            if pri then <<write"565656 ph=",ph;terpri()>>;
          >>                                     else
          if vari then ph:=nil
                  else
          if facint_ then <<
            ph:=findintfac(list(qh),ftem,vl,v,doneintvar,intfacdep,
                           not zerop n,not potflag);
            % factorize before ivestig., no report of int. factors
            if ph then << %--- Complete integr. of qh was possible
              if pri then write"of the homogeneous part"$terpri()$
              %--- update the list of variables on which all integr.
              %--- factors depend apart from the integration variable
              intfacdepnew:=caddr ph;
              %--- extend the list of integrating factors, cadr ph
              %--- is a list of integr. factors, here only one
              intfaclist:=cons(caadr ph,intfaclist);
              %--- multiply the inhomogeneous part with integ. factor
              qih:=reval reval reval list('TIMES,car intfaclist,qih);
              if pri then <<write"454545 qih=",qih;terpri()>>;
              ph:=car ph  % the integral of qh
            >>
          >>;

          %------ Now the integration of the inhomogeneous part
          if not ph then pih:=nil %--- no integration possible
                    else <<
            if zerop qih then pih:=list(0,0) else
            pih:=intpde(qih,ftem,vl,v,partial)$

            if print_ and null pih then <<
              terpri()$
              write"Inhomogeneous part: "$
              mathprint qih$
              write"can not be integrated explicitly wrt. ",v$
            >>$
            if pri then <<write"nach intpde(qih):",pih$terpri()$
                          write"genflag=",genflag$terpri()>>$

            if pih then
            if zerop cadr pih then
            <<qih:=car pih$n:=add1 n$iflag:='success$
              if pri then write" success "$
            >>
                              else if not genflag then pih:=nil
                                                  else
            <<if pri then write"555"$
              geni:=partint(cadr pih,smemberl(ftem,cadr pih),
                            vl,v,genflag)$
              if geni then
              <<qih:=reval list('PLUS,car pih,car geni)$
                n:=add1 n$
                ftem:=union(fnew_,ftem)$
                newcond:=append(cdr geni,newcond)$  % additional de's
                if pri then
                <<terpri()$write"nach gen newcond:",newcond>>$
                iflag:='genint
              >>                       else
              if partial then qih:=car pih
                         else pih:=nil
            >>;
            if pih then <<
              if pri then write"AAA"$
               qh:=ph;
              if (not potflag) and (n neq 1) then
              intfacdep:=intfacdepnew
              %-The first integr. factor of all v-integrations does not
              % depend on any earlier integration variables and can
              % therefore be taken out of all integrals --> no incease
              % of intfacdep for n=1.
              %-For potential integration the integration variables and
              % extra-dependencies-variables of integr. factors need not
              % be disjunct therefore the intfacdep-update only for
              % not-potflag
            >>     else <<
              if pri then write"BBB"$
              % inhomogeneous part can not be integrated therefore
              % reversing the succesful integration of the hom. part
              if car intfaclist neq 1 then
              qih:=reval list('QUOTIENT,qih,car intfaclist);
              intfaclist:=cdr intfaclist
            >>;
          >>; %-- end of the integration of the inhomog. part
          if pri then write"n=",n," nmax=",nmax," not pih=",not pih$
        >> until (n=nmax) or (not pih)$ %---- end of v-integration
        %------- variables of done integrations are collected for
        %------- determining integrating factors in later integr.
        if not zerop n then doneintvar:=cons(v,doneintvar)$
        nges:=nges+n;
        intlist:=cons(intfaclist,intlist)
      >>$  % of    not ( vari and (not member(v,vl)))
      if vari then <<vari:=nil;vlcop:=nil>>;
      if pri then write"ende: intvar=",intvar," vari=",vari,
      "    qh=",qh,"   qih=",qih$
    >> % of (nmax neq zero)
  >>$  % of ( while (vari or vlcop) and (pih or (not potflag)) )

  % intlist and its elements intfaclist are in the inverse order
  % to vl and the sequence of integrations done
  q:=reval list('PLUS,qh,qih)$ %--- adding homog. and inhomog. part
  if pri then <<terpri()$write"\\\  newcond:"$deprint newcond;
    write"multiple result:",if null iflag then nil
    else list(q,intlist,newcond,nges)
  >>;
  return if null iflag then nil
                       else list(q,intlist,newcond,nges)
end$  % of multipleint

symbolic procedure uplistoflds(intlist,listoflds)$
begin
  scalar f,h1,h2,h3,h4,lds,itl;
  while listoflds do
  <<f:=caar listoflds;
    lds:=cdar listoflds;
    listoflds:=cdr listoflds;
    h2:=nil;            % h2 becomes the new list of lds of f
    while lds do
    <<h3:=car lds; lds:=cdr lds;
      itl:=intlist;
      h4:=nil;          % h4 becomes one new ld of f
      while h3 do
      <<h4:=cons(car h3 - if null car itl then 0
                                          else length car itl, h4);
        h3:=cdr h3;itl:=cdr itl
      >>;
      h2:=cons(reverse h4,h2)
    >>;
    h1:=cons(cons(f,h2),h1)
  >>;
  return h1  % updated listoflds
end$ % of uplistoflds

symbolic procedure addintco(q, ftem, ifac, vl, vari)$
begin scalar v,f,l,vl1;
  % multi.ing factors to the constants/functions of integration
  if zerop q then l:=1
             else
  <<ftem:=fctsort ftem$
    while ftem do
    if fctlength car ftem<length vl then ftem:=nil
                                    else if fctlinear(q,f)          then
                                         <<f:=car ftem$ ftem:=nil>> else
                                         ftem:=cdr ftem$
    if f then
    <<l:=lderiv(q,f,fctargs f)$
      l:=reval coeffn(q,reval car l,cdr l)
    >>   else l:=1
  >>;
  % the constants and functions of integration
  if vari then q:=list('PLUS,q,intconst(l,vl,vari,list(1)))
          else
  <<vl1:=vl;
    while vl1 do
    <<v:=car vl1; vl1:=cdr vl1;
      if car ifac then
      q:=list('PLUS,q,intconst(l,vl,v,car ifac))$
      % l..product of factors in the coefficient of the function to be
      % eliminated, car ifac .. list of integrating factors
      ifac:=cdr ifac;
    >>
  >>$
  return reval q
end$ % of addintco

symbolic procedure integratepde(p,ftem,vari,genflag,potflag)$
%  Generalized integration of the expression p
%     if not genflag then "normal integration"
%  Equation p must not be directly separable, otherwise the depen-
%  dencies of functions of integration on their variables is wrong,
%  i.e. no dependence on explicit variables
%  ftem are all functions from the `global' ftem which occur in p, i.e.
%  ftem:=smemberl(ftem,p)$
%  if vari=nil then integration w.r.t. all possible variables within
%                   the equation
%              else only w.r.t. vari one time

begin
  scalar vl,vlrev,v,intlist,
  ili1a,ili2a,maxvanz,fsub,h,hh,nfsub,iflag,newcond,
  n1,n2,pot1,pot2,p1,p2,listoflds,secnd,ifac0,
  ifac1a,ifac1b,ifac2a,ifac2b,cop,v1a,v2a,pri$

  % pri:=t;
  if pri then <<terpri()$write"Start Integratepde">>$
  vl:=argset ftem$
  vlrev:=reverse vl;
  if vari then <<potflag:=nil;
                 if zerop p then iflag:='success>>
          else
  <<%------- determining fsub=list of functions of all variables
    maxvanz:=length vl$
    fsub:=nil;
    h:=ftem;
    while h do
    <<if fctlength car h=maxvanz then
      fsub:=cons(car h,fsub)$
      h:=cdr h
    >>$
    nfsub:=length fsub$  % must be >1 for potential-integration
    h:=intminderiv(p,ftem,vlrev,maxvanz,nfsub)$ % fsub is also for below
    intlist:=car h$
    %-- list of necessary integrations of the whole equation to solve
    %-- for a function of all variables
    listoflds:=cadr h$ %-- list of leading derivatives
  >>$
  if pri then <<terpri()$
                write"complete integrations:",intlist," for:",vl>>;
  %-- n1 is the number of integrations which must be done to try
  %-- potential integration which must enable to eliminate 2 functions
  %-- n2 is the number of integrations actually done
  n1:=for each h in intlist sum h;
  if (not vari) and (zerop n1) then
  <<n2:=0;
    if potflag then % else not necessary
    for h:=1:(length vl) do ifac0:=cons(nil,ifac0)
  >>                           else
  <<if tr_genint then
    <<terpri()$write "integration of the expression : "$
      eqprint p>>$
    if pri then
    <<terpri()$write"at first all multiple complete integration">>;
    %-- At first if possible n2 integrations of the whole equation
    h:=multipleint(intlist,ftem,p,vari,vl,genflag,nil,nil,nil)$
                   % potflag=nil, partial=nil, doneintvar=nil
    if h then
    <<p:=car h;
      ifac0:=cadr h;  % ifac0 is the list of lists of integr. factors
      newcond:=caddr h;
      n2:=cadddr h;   % number of done integrations
      % doneintvar & intfacdep for the two halfs of integrations
      % from the two parts of ifac0
      h:=nil;
      iflag:='success;
    >>   else n2:=0;
    ftem:=union(fnew_,ftem)$
  >>;
  %------------ Existence of a potential ?
  if (n1=n2) and potflag and (nfsub>1) then
  %---- at least 2 functions to solve for
  <<if not zerop n2 then            %---- update listoflds
    listoflds:=uplistoflds(reverse ifac0,listoflds)$
    if pri then <<terpri()$write"uplistoflds:",listoflds>>$
    if h:=potintegrable(listoflds) then
    <<ili1a:=car h; ili2a:=cadr h;
      % The necess. differentiations of the potential
      if pri then
      <<terpri()$write"potintegrable:",ili1a,"  ",ili2a>>$

      if pri then <<write"+++ intlist=",intlist,
                           "    ili1a=",ili1a,
                           "    ili2a=",ili2a>>$
      %-- distributing the integrating factors of ifac0 among
      %-- the two lists ifac1b and ifac2b which are so far nil
      %-- such that (ifac1b and ili1a are disjunct) and
      %--           (ifac2b and ili2a are disjunct)
      v1a:=vlofintlist(vl,ili1a);
      v2a:=vlofintlist(vl,ili2a);

      hh:=t;
      cop:=reverse ifac0;
      ifac1a:=ili1a;ifac2a:=ili2a;
      while hh and cop do <<
        % cop is a list of lists of integr. factors
        if car cop then h:=vlofintfaclist(vl,cdar cop)
                   else h:=nil;
        if freeoflist(h,v2a) and (car ifac2a=0) then <<
          ifac1b:=cons( nil, ifac1b);
          ifac2b:=cons( reverse car cop, ifac2b)
        >>                   else
        if freeoflist(h,v1a) and (car ifac1a=0) then <<
          ifac2b:=cons( nil, ifac2b);
          ifac1b:=cons( reverse car cop, ifac1b)
        >>                   else
        if car cop then hh:=nil;
        ifac1a:=cdr ifac1a;
        ifac2a:=cdr ifac2a;
        cop:=cdr cop;
      >>;
      % the elements of ifac1b,ifac2b are in reverse order to
      % ifac1a,ifac2a and are in the same order as vl, also
      % the elements in the infac-lists are in inverse order,
      % i.e. in the order the integrations have been done
      if pri then <<terpri()$
                    write  "ifac1a=",ifac1a,"  ifac1b=",ifac1b,
                    "  ifac2a=",ifac2a,"  ifac2b=",ifac2b >>$

      %-- lists of integrations to be done to both parts
      if hh then
      repeat % possibly a second try with part2 integrated first
      <<n1:=for each n1 in ili1a sum n1;
        % n1 .. number of remaining integrations of the first half
        p1:=multipleint(ili1a,ftem,p,nil,vl,genflag,t,t,
                        % potflag=t, partial=t
                        union(vlofintlist(vl,ili2a),
                              vlofintlist(vl,ifac1b)))$
        % that the variables of integration are not in ifac1b
        % was already checked. Only restriction: the integrating
        % factors must not depend on the last argument.

        ftem:=union(fnew_,ftem)$
        if p1 then <<
          ifac1a:=cadr p1;
          % ifac1a is now the list of integrating factors
          if newcond then newcond:=nconc(newcond,caddr p1)
                     else newcond:=caddr p1;
          if pri then <<terpri()$write"mul2: newcond=",newcond>>$
          n2:=cadddr p1;
          p1:=car p1
        >>;
        if p1 and (n1=n2) then
        %--- if the first half has been integrated suff. often
        <<%--- integrating the second half sufficiently often
          n1:=for each n1 in ili2a sum n1;
          % calculation of the 2. part which is not contained in p1
          p2:=p1;
          cop:=ifac1a; hh:=vlrev; % because ifac1a is reversed
          while cop do <<
            h:=car cop;cop:=cdr cop;
            v:=car hh;hh:=cdr hh;
            % h is the list of integrating factors of the v-integr.
            while h do <<
              p2:=reval list('QUOTIENT,list('DF,p2,v),car h);
              h:=cdr h
            >>
          >>;
          p2:=reval reval list('PLUS,p,list('MINUS,p2));
          p2:=multipleint(ili2a,ftem,p2,nil,vl,genflag,t,nil,
                          % potflag=t, partial=nil
                          union(vlofintlist(vl,ili1a),
                                vlofintlist(vl,ifac2b)))$
          ftem:=union(fnew_,ftem)$
          if p2 then <<
            ifac2a:=cadr p2;
            % ifac2a is now list of integrating factors
            if newcond then newcond:=nconc(newcond,caddr p2)
                       else newcond:=caddr p2;
            if pri then <<terpri()$write"mul3: newcond=",newcond>>$
            n2:=cadddr p2;
            p2:=car p2
          >>;
          if p2 and (n1=n2) then
          % if the second half has been integrated sufficiently often
          <<% can both halfes be solved for different functions
            % i.e. are inhomogeneous
  % part which is of advantage for finding integrating factors
  q:=splitinhom(q,ftem,vl)$
  qh:=car q; qih:=cdr q; q:=nil;

  while (vari or vlcop) and (pih or (not potflag)) do
  %------- if for potflag=t one variable can not be integrated the
  %------- maximal number of times (nmax) then immediate stop because
  %------- then no elimination of two functions will be possible
  << %-- The next integration variable: v, no of integrations: nmax
    if vari then <<v:=vari;nmax:=1>>
            else <<v:=car vlcop;     vlcop:=cdr vlcop;
                   nmax:=car intvar; intvar:=cdr intvar>>;

    if zerop nmax then intlist:=cons(nil,intlist)
                  else
    <<if pri then write"anf: intvar=",intvar," vari=",vari,"    q=",q$
      if vari and (not member(v,vl)) then
      <<qh :=reval list('INT,qh ,v)$
        if freeof(qh,'INT) then <<
          qih:=reval list('INT,qih,v)$
          iflag:=if freeint_ and
                    (null freeof(qih,'INT)) then nil else
                 if freeabs_ and
                    (null freeof(qih,'ABS)) then nil else <<
                   intlist:=cons(list(1),intlist)$
                   'success>>$
          if pri then <<write"232323 qh=",qh;terpri();
                        write"qih=",qih;terpri()>>
        >>
      >>                             else
      <<n:=0$
        if pri then write"333"$
        intfaclist:=nil; %-- the list of integr. factors in v-integr.
        if potflag or my_freeof(intfacdep,v) then
        % otherwise v-integration not allowed because one integrating
        % factor already depends on v
        % for potflag=t this `commutativity'-demand plays no role
        repeat << %--- max nmax integrations of qh and qih wrt. v
          if pri then <<write"444  vor intpde:"$eqprint q$terpri()$
                        write"potflag=",potflag," v=",v,
                        "  ftem=",ftem>>$
          % At first trying a direct integration of the homog. part qh
          ph:=intpde(qh,ftem,vl,v,partial)$  % faster if potflag=nil
          if pri then <<write"nach intpde(qh):"$deprint ph>>$

          %------ At first the integration of the homogeneous part
          intfacdepnew:=intfacdep;
          if ph and (partial or (zerop cadr ph)) then <<
            %---- For the homogen. part cadr ph must be zero
            intfaclist:=cons(1,intfaclist);
            ph:=car ph;
            if pri then <<write"565656 ph=",ph;terpri()>>;
          >>                                     else
          if vari then ph:=nil
                  else
          if facint_ then <<
            ph:=findintfac(list(qh),ftem,vl,v,doneintvar,intfacdep,
                           not zerop n,not potflag);
            % factorize before ivestig., no report of int. factors
            if ph then << %--- Complete integr. of qh was possible
              if pri then write"of the homogeneous part"$terpri()$
              %--- update the list of variables on which all integr.
              %--- factors depend apart from the integration variable
              intfacdepnew:=caddr ph;
              %--- extend the list of integrating factors, cadr ph
              %--- is a list of integr. factors, here only one
              intfaclist:=cons(caadr ph,intfaclist);
              %--- multiply the inhomogeneous part with integ. factor
              qih:=reval reval reval list('TIMES,car intfaclist,qih);
              if pri then <<write"454545 qih=",qih;terpri()>>;
              ph:=car ph  % the integral of qh
            >>
          >>;

          %------ Now the integration of the inhomogeneous part
          if not ph then pih:=nil %--- no integration possible
                    else <<
            if zerop qih then pih:=list(0,0) else
            pih:=intpde(qih,ftem,vl,v,partial)$

            if print_ and null pih then <<
              terpri()$
              write"Inhomogeneous part: "$
              mathprint qih$
              write"can not be integrated explicitly wrt. ",v$
            >>$
            if pri then <<write"nach intpde(qih):",pih$terpri()$
                          write"genflag=",genflag$terpri()>>$

            if pih then
            if zerop cadr pih then
            <<qih:=car pih$n:=add1 n$iflag:='success$
              if pri then write" success "$
            >>
                              else if not genflag then pih:=nil
                                                  else
            <<if pri then write"555"$
              geni:=partint(cadr pih,smemberl(ftem,cadr pih),
                            vl,v,genflag)$
              if geni then
              <<qih:=reval list('PLUS,car pih,car geni)$
                n:=add1 n$
                ftem:=union(fnew_,ftem)$
                newcond:=append(cdr geni,newcond)$  % additional de's
                if pri then
                <<terpri()$write"nach gen newcond:",newcond>>$
                iflag:='genint
              >>                       else
              if partial then qih:=car pih
                         else pih:=nil
            >>;
            if pih then <<
              if pri then write"AAA"$
               qh:=ph;
              if (not potflag) and (n neq 1) then
              intfacdep:=intfacdepnew
              %-The first integr. factor of all v-integrations does not
              % depend on any earlier integration variables and can
              % therefore be taken out of all integrals --> no incease
              % of intfacdep for n=1.
              %-For potential integration the integration variables and
              % extra-dependencies-variables of integr. factors need not
              % be disjunct therefore the intfacdep-update only for
              % not-potflag
            >>     else <<
              if pri then write"BBB"$
              % inhomogeneous part can not be integrated therefore
              % reversing the succesful integration of the hom. part
              if car intfaclist neq 1 then
              qih:=reval list('QUOTIENT,qih,car intfaclist);
              intfaclist:=cdr intfaclist
            >>;
          >>; %-- end of the integration of the inhomog. part
          if pri then write"n=",n," nmax=",nmax," not pih=",not pih$
        >> until (n=nmax) or (not pih)$ %---- end of v-integration
        %------- variables of done integrations are collected for
        %------- determining integrating factors in later integr.
        if not zerop n then doneintvar:=cons(v,doneintvar)$
        nges:=nges+n;
        intlist:=cons(intfaclist,intlist)
      >>$  % of    not ( vari and (not member(v,vl)))
      if vari then <<vari:=nil;vlcop:=nil>>;
      if pri then write"ende: intvar=",intvar," vari=",vari,
      "    qh=",qh,"   qih=",qih$
    >> % of (nmax neq zero)
  >>$  % of ( while (vari or vlcop) and (pih or (not potflag)) )

  % intlist and its elements intfaclist are in the inverse order
  % to vl and the sequence of integrations done
  q:=reval list('PLUS,qh,qih)$ %--- adding homog. and inhomog. part
  if pri then <<terpri()$write"\\\  newcond:"$deprint newcond;
    write"multiple result:",if null iflag then nil
    else list(q,intlist,newcond,nges)
  >>;
  return if null iflag then nil
                       else list(q,intlist,newcond,nges)
end$  % of multipleint

symbolic procedure uplistoflds(intlist,listoflds)$
begin
  scalar f,h1,h2,h3,h4,lds,itl;
  while listoflds do
  <<f:=caar listoflds;
    lds:=cdar listoflds;
    listoflds:=cdr listoflds;
    h2:=nil;            % h2 becomes the new list of lds of f
    while lds do
    <<h3:=car lds; lds:=cdr lds;
      itl:=intlist;
      h4:=nil;          % h4 becomes one new ld of f
      while h3 do
      <<h4:=cons(car h3 - if null car itl then 0
                                          else length car itl, h4);
        h3:=cdr h3;itl:=cdr itl
      >>;
      h2:=cons(reverse h4,h2)
    >>;
    h1:=cons(cons(f,h2),h1)
  >>;
  return h1  % updated listoflds
end$ % of uplistoflds

symbolic procedure addintco(q, ftem, ifac, vl, vari)$
begin scalar v,f,l,vl1;
  % multi.ing factors to the constants/functions of integration
  if zerop q then l:=1
             else
  <<ftem:=fctsort ftem$
    while ftem do
    if fctlength car ftem<length vl then ftem:=nil
                                    else if fctlinear(q,f)          then
                                         <<f:=car ftem$ ftem:=nil>> else
                                         ftem:=cdr ftem$
    if f then
    <<l:=lderiv(q,f,fctargs f)$
      l:=reval coeffn(q,reval car l,cdr l)
    >>   else l:=1
  >>;
  % the constants and functions of integration
  if vari then q:=list('PLUS,q,intconst(l,vl,vari,list(1)))
          else
  <<vl1:=vl;
    while vl1 do
    <<v:=car vl1; vl1:=cdr vl1;
      if car ifac then
      q:=list('PLUS,q,intconst(l,vl,v,car ifac))$
      % l..product of factors in the coefficient of the function to be
      % eliminated, car ifac .. list of integrating factors
      ifac:=cdr ifac;
    >>
  >>$
  return reval q
end$ % of addintco

symbolic procedure integratepde(p,ftem,vari,genflag,potflag)$
%  Generalized integration of the expression p
%     if not genflag then "normal integration"
%  Equation p must not be directly separable, otherwise the depen-
%  dencies of functions of integration on their variables is wrong,
%  i.e. no dependence on explicit variables
%  ftem are all functions from the `global' ftem which occur in p, i.e.
%  ftem:=smemberl(ftem,p)$
%  if vari=nil then integration w.r.t. all possible variables within
%                   the equation
%              else only w.r.t. vari one time

begin
  scalar vl,vlrev,v,intlist,
  ili1a,ili2a,maxvanz,fsub,h,hh,nfsub,iflag,newcond,
  n1,n2,pot1,pot2,p1,p2,listoflds,secnd,ifac0,
  ifac1a,ifac1b,ifac2a,ifac2b,cop,v1a,v2a,pri$

  % pri:=t;
  if pri then <<terpri()$write"Start Integratepde">>$
  vl:=argset ftem$
  vlrev:=reverse vl;
  if vari then <<potflag:=nil;
                 if zerop p then iflag:='success>>
          else
  <<%------- determining fsub=list of functions of all variables
    maxvanz:=length vl$
    fsub:=nil;
    h:=ftem;
    while h do
    <<if fctlength car h=maxvanz then
      fsub:=cons(car h,fsub)$
      h:=cdr h
    >>$
    nfsub:=length fsub$  % must be >1 for potential-integration
    h:=intminderiv(p,ftem,vlrev,maxvanz,nfsub)$ % fsub is also for below
    intlist:=car h$
    %-- list of necessary integrations of the whole equation to solve
    %-- for a function of all variables
    listoflds:=cadr h$ %-- list of leading derivatives
  >>$
  if pri then <<terpri()$
                write"complete integrations:",intlist," for:",vl>>;
  %-- n1 is the number of integrations which must be done to try
  %-- potential integration which must enable to eliminate 2 functions
  %-- n2 is the number of integrations actually done
  n1:=for each h in intlist sum h;
  if (not vari) and (zerop n1) then
  <<n2:=0;
    if potflag then % else not necessary
    for h:=1:(length vl) do ifac0:=cons(nil,ifac0)
  >>                           else
  <<if tr_genint then
    <<terpri()$write "integration of the expression : "$
      eqprint p>>$
    if pri then
    <<terpri()$write"at first all multiple complete integration">>;
    %-- At first if possible n2 integrations of the whole equation
    h:=multipleint(intlist,ftem,p,vari,vl,genflag,nil,nil,nil)$
                   % potflag=nil, partial=nil, doneintvar=nil
    if h then
    <<p:=car h;
      ifac0:=cadr h;  % ifac0 is the list of lists of integr. factors
      newcond:=caddr h;
      n2:=cadddr h;   % number of done integrations
      % doneintvar & intfacdep for the two halfs of integrations
      % from the two parts of ifac0
      h:=nil;
      iflag:='success;
    >>   else n2:=0;
    ftem:=union(fnew_,ftem)$
  >>;
  %------------ Existence of a potential ?
  if (n1=n2) and potflag and (nfsub>1) then
  %---- at least 2 functions to solve for
  <<if not zerop n2 then            %---- update listoflds
    listoflds:=uplistoflds(reverse ifac0,listoflds)$
    if pri then <<terpri()$write"uplistoflds:",listoflds>>$
    if h:=potintegrable(listoflds) then
    <<ili1a:=car h; ili2a:=cadr h;
      % The necess. differentiations of the potential
      if pri then
      <<terpri()$write"potintegrable:",ili1a,"  ",ili2a>>$

      if pri then <<write"+++ intlist=",intlist,
                           "    ili1a=",ili1a,
                           "    ili2a=",ili2a>>$
      %-- distributing the integrating factors of ifac0 among
      %-- the two lists ifac1b and ifac2b which are so far nil
      %-- such that (ifac1b and ili1a are disjunct) and
      %--           (ifac2b and ili2a are disjunct)
      v1a:=vlofintlist(vl,ili1a);
      v2a:=vlofintlist(vl,ili2a);

      hh:=t;
      cop:=reverse ifac0;
      ifac1a:=ili1a;ifac2a:=ili2a;
      while hh and cop do <<
        % cop is a list of lists of integr. factors
        if car cop then h:=vlofintfaclist(vl,cdar cop)
                   else h:=nil;
        if freeoflist(h,v2a) and (car ifac2a=0) then <<
          ifac1b:=cons( nil, ifac1b);
          ifac2b:=cons( reverse car cop, ifac2b)
        >>                   else
        if freeoflist(h,v1a) and (car ifac1a=0) then <<
          ifac2b:=cons( nil, ifac2b);
          ifac1b:=cons( reverse car cop, ifac1b)
        >>                   else
        if car cop then hh:=nil;
        ifac1a:=cdr ifac1a;
        ifac2a:=cdr ifac2a;
        cop:=cdr cop;
      >>;
      % the elements of ifac1b,ifac2b are in reverse order to
      % ifac1a,ifac2a and are in the same order as vl, also
      % the elements in the infac-lists are in inverse order,
      % i.e. in the order the integrations have been done
      if pri then <<terpri()$
                    write  "ifac1a=",ifac1a,"  ifac1b=",ifac1b,
                    "  ifac2a=",ifac2a,"  ifac2b=",ifac2b >>$

      %-- lists of integrations to be done to both parts
      if hh then
      repeat % possibly a second try with part2 integrated first
      <<n1:=for each n1 in ili1a sum n1;
        % n1 .. number of remaining integrations of the first half
        p1:=multipleint(ili1a,ftem,p,nil,vl,genflag,t,t,
                        % potflag=t, partial=t
                        union(vlofintlist(vl,ili2a),
                              vlofintlist(vl,ifac1b)))$
        % that the variables of integration are not in ifac1b
        % was already checked. Only restriction: the integrating
        % factors must not depend on the last argument.

        ftem:=union(fnew_,ftem)$
        if p1 then <<
          ifac1a:=cadr p1;
          % ifac1a is now the list of integrating factors
          if newcond then newcond:=nconc(newcond,caddr p1)
                     else newcond:=caddr p1;
          if pri then <<terpri()$write"mul2: newcond=",newcond>>$
          n2:=cadddr p1;
          p1:=car p1
        >>;
        if p1 and (n1=n2) then
        %--- if the first half has been integrated suff. often
        <<%--- integrating the second half sufficiently often
          n1:=for each n1 in ili2a sum n1;
          % calculation of the 2. part which is not contained in p1
          p2:=p1;
          cop:=ifac1a; hh:=vlrev; % because ifac1a is reversed
          while cop do <<
            h:=car cop;cop:=cdr cop;
            v:=car hh;hh:=cdr hh;
            % h is the list of integrating factors of the v-integr.
            while h do <<
              p2:=reval list('QUOTIENT,list('DF,p2,v),car h);
              h:=cdr h
            >>
          >>;
          p2:=reval reval list('PLUS,p,list('MINUS,p2));
          p2:=multipleint(ili2a,ftem,p2,nil,vl,genflag,t,nil,
                          % potflag=t, partial=nil
                          union(vlofintlist(vl,ili1a),
                                vlofintlist(vl,ifac2b)))$
          ftem:=union(fnew_,ftem)$
          if p2 then <<
            ifac2a:=cadr p2;
            % ifac2a is now list of integrating factors
            if newcond then newcond:=nconc(newcond,caddr p2)
                       else newcond:=caddr p2;
            if pri then <<terpri()$write"mul3: newcond=",newcond>>$
            n2:=cadddr p2;
            p2:=car p2
          >>;
          if p2 and (n1=n2) then
          % if the second half has been integrated sufficiently often
          <<% can both halfes be solved for different functions
            % i.e. are inhomogeneous
  % part which is of advantage for finding integrating factors
  q:=splitinhom(q,ftem,vl)$
  qh:=car q; qih:=cdr q; q:=nil;

  while (vari or vlcop) and (pih or (not potflag)) do
  %------- if for potflag=t one variable can not be integrated the
  %------- maximal number of times (nmax) then immediate stop because
  %------- then no elimination of two functions will be possible
  << %-- The next integration variable: v, no of integrations: nmax
    if vari then <<v:=vari;nmax:=1>>
            else <<v:=car vlcop;     vlcop:=cdr vlcop;
                   nmax:=car intvar; intvar:=cdr intvar>>;

    if zerop nmax then intlist:=cons(nil,intlist)
                  else
    <<if pri then write"anf: intvar=",intvar," vari=",vari,"    q=",q$
      if vari and (not member(v,vl)) then
      <<qh :=reval list('INT,qh ,v)$
        if freeof(qh,'INT) then <<
          qih:=reval list('INT,qih,v)$
          iflag:=if freeint_ and
                    (null freeof(qih,'INT)) then nil else
                 if freeabs_ and
                    (null freeof(qih,'ABS)) then nil else <<
                   intlist:=cons(list(1),intlist)$
                   'success>>$
          if pri then <<write"232323 qh=",qh;terpri();
                        write"qih=",qih;terpri()>>
        >>
      >>                             else
      <<n:=0$
        if pri then write"333"$
        intfaclist:=nil; %-- the list of integr. factors in v-integr.
        if potflag or my_freeof(intfacdep,v) then
        % otherwise v-integration not allowed because one integrating
        % factor already depends on v
        % for potflag=t this `commutativity'-demand plays no role
        repeat << %--- max nmax integrations of qh and qih wrt. v
          if pri then <<write"444  vor intpde:"$eqprint q$terpri()$
                        write"potflag=",potflag," v=",v,
                        "  ftem=",ftem>>$
          % At first trying a direct integration of the homog. part qh
          ph:=intpde(qh,ftem,vl,v,partial)$  % faster if potflag=nil
          if pri then <<write"nach intpde(qh):"$deprint ph>>$

          %------ At first the integration of the homogeneous part
          intfacdepnew:=intfacdep;
          if ph and (partial or (zerop cadr ph)) then <<
            %---- For the homogen. part cadr ph must be zero
            intfaclist:=cons(1,intfaclist);
            ph:=car ph;
            if pri then <<write"565656 ph=",ph;terpri()>>;
          >>                                     else
          if vari then ph:=nil
                  else
          if facint_ then <<
            ph:=findintfac(list(qh),ftem,vl,v,doneintvar,intfacdep,
                           not zerop n,not potflag);
            % factorize before ivestig., no report of int. factors
            if ph then << %--- Complete integr. of qh was possible
              if pri then write"of the homogeneous part"$terpri()$
              %--- update the list of variables on which all integr.
              %--- factors depend apart from the integration variable
              intfacdepnew:=caddr ph;
              %--- extend the list of integrating factors, cadr ph
              %--- is a list of integr. factors, here only one
              intfaclist:=cons(caadr ph,intfaclist);
              %--- multiply the inhomogeneous part with integ. factor
              qih:=reval reval reval list('TIMES,car intfaclist,qih);
              if pri then <<write"454545 qih=",qih;terpri()>>;
              ph:=car ph  % the integral of qh
            >>
          >>;

          %------ Now the integration of the inhomogeneous part
          if not ph then pih:=nil %--- no integration possible
                    else <<
            if zerop qih then pih:=list(0,0) else
            pih:=intpde(qih,ftem,vl,v,partial)$

            if print_ and null pih then <<
              terpri()$
              write"Inhomogeneous part: "$
              mathprint qih$
              write"can not be integrated explicitly wrt. ",v$
            >>$
            if pri then <<write"nach intpde(qih):",pih$terpri()$
                          write"genflag=",genflag$terpri()>>$

            if pih then
            if zerop cadr pih then
            <<qih:=car pih$n:=add1 n$iflag:='success$
              if pri then write" success "$
            >>
                              else if not genflag then pih:=nil
                                                  else
            <<if pri then write"555"$
              geni:=partint(cadr pih,smemberl(ftem,cadr pih),
                            vl,v,genflag)$
              if geni then
              <<qih:=reval list('PLUS,car pih,car geni)$
                n:=add1 n$
                ftem:=union(fnew_,ftem)$
                newcond:=append(cdr geni,newcond)$  % additional de's
                if pri then
                <<terpri()$write"nach gen newcond:",newcond>>$
                iflag:='genint
              >>                       else
              if partial then qih:=car pih
                         else pih:=nil
            >>;
            if pih then <<
              if pri then write"AAA"$
               qh:=ph;
              if (not potflag) and (n neq 1) then
              intfacdep:=intfacdepnew
              %-The first integr. factor of all v-integrations does not
              % depend on any earlier integration variables and can
              % therefore be taken out of all integrals --> no incease
              % of intfacdep for n=1.
              %-For potential integration the integration variables and
              % extra-dependencies-variables of integr. factors need not
              % be disjunct therefore the intfacdep-update only for
              % not-potflag
            >>     else <<
              if pri then write"BBB"$
              % inhomogeneous part can not be integrated therefore
              % reversing the succesful integration of the hom. part
              if car intfaclist neq 1 then
              qih:=reval list('QUOTIENT,qih,car intfaclist);
              intfaclist:=cdr intfaclist
            >>;
          >>; %-- end of the integration of the inhomog. part
          if pri then write"n=",n," nmax=",nmax," not pih=",not pih$
        >> until (n=nmax) or (not pih)$ %---- end of v-integration
        %------- variables of done integrations are collected for
        %------- determining integrating factors in later integr.
        if not zerop n then doneintvar:=cons(v,doneintvar)$
        nges:=nges+n;
        intlist:=cons(intfaclist,intlist)
      >>$  % of    not ( vari and (not member(v,vl)))
      if vari then <<vari:=nil;vlcop:=nil>>;
      if pri then write"ende: intvar=",intvar," vari=",vari,
      "    qh=",qh,"   qih=",qih$
    >> % of (nmax neq zero)
  >>$  % of ( while (vari or vlcop) and (pih or (not potflag)) )

  % intlist and its elements intfaclist are in the inverse order
  % to vl and the sequence of integrations done
  q:=reval list('PLUS,qh,qih)$ %--- adding homog. and inhomog. part
  if pri then <<terpri()$write"\\\  newcond:"$deprint newcond;
    write"multiple result:",if null iflag then nil
    else list(q,intlist,newcond,nges)
  >>;
  return if null iflag then nil
                       else list(q,intlist,newcond,nges)
end$  % of multipleint

symbolic procedure uplistoflds(intlist,listoflds)$
begin
  scalar f,h1,h2,h3,h4,lds,itl;
  while listoflds do
  <<f:=caar listoflds;
    lds:=cdar listoflds;
    listoflds:=cdr listoflds;
    h2:=nil;            % h2 becomes the new list of lds of f
    while lds do
    <<h3:=car lds; lds:=cdr lds;
      itl:=intlist;
      h4:=nil;          % h4 becomes one new ld of f
      while h3 do
      <<h4:=cons(car h3 - if null car itl then 0
                                          else length car itl, h4);
        h3:=cdr h3;itl:=cdr itl
      >>;
      h2:=cons(reverse h4,h2)
    >>;
    h1:=cons(cons(f,h2),h1)
  >>;
  return h1  % updated listoflds
end$ % of uplistoflds

symbolic procedure addintco(q, ftem, ifac, vl, vari)$
begin scalar v,f,l,vl1;
  % multi.ing factors to the constants/functions of integration
  if zerop q then l:=1
             else
  <<ftem:=fctsort ftem$
    while ftem do
    if fctlength car ftem<length vl then ftem:=nil
                                    else if fctlinear(q,f)          then
                                         <<f:=car ftem$ ftem:=nil>> else
                                         ftem:=cdr ftem$
    if f then
    <<l:=lderiv(q,f,fctargs f)$
      l:=reval coeffn(q,reval car l,cdr l)
    >>   else l:=1
  >>;
  % the constants and functions of integration
  if vari then q:=list('PLUS,q,intconst(l,vl,vari,list(1)))
          else
  <<vl1:=vl;
    while vl1 do
    <<v:=car vl1; vl1:=cdr vl1;
      if car ifac then
      q:=list('PLUS,q,intconst(l,vl,v,car ifac))$
      % l..product of factors in the coefficient of the function to be
      % eliminated, car ifac .. list of integrating factors
      ifac:=cdr ifac;
    >>
  >>$
  return reval q
end$ % of addintco

symbolic procedure integratepde(p,ftem,vari,genflag,potflag)$
%  Generalized integration of the expression p
%     if not genflag then "normal integration"
%  Equation p must not be directly separable, otherwise the depen-
%  dencies of functions of integration on their variables is wrong,
%  i.e. no dependence on explicit variables
%  ftem are all functions from the `global' ftem which occur in p, i.e.
%  ftem:=smemberl(ftem,p)$
%  if vari=nil then integration w.r.t. all possible variables within
%                   the equation
%              else only w.r.t. vari one time

begin
  scalar vl,vlrev,v,intlist,
  ili1a,ili2a,maxvanz,fsub,h,hh,nfsub,iflag,newcond,
  n1,n2,pot1,pot2,p1,p2,listoflds,secnd,ifac0,
  ifac1a,ifac1b,ifac2a,ifac2b,cop,v1a,v2a,pri$

  % pri:=t;
  if pri then <<terpri()$write"Start Integratepde">>$
  vl:=argset ftem$
  vlrev:=reverse vl;
  if vari then <<potflag:=nil;
                 if zerop p then iflag:='success>>
          else
  <<%------- determining fsub=list of functions of all variables
    maxvanz:=length vl$
    fsub:=nil;
    h:=ftem;
    while h do
    <<if fctlength car h=maxvanz then
      fsub:=cons(car h,fsub)$
      h:=cdr h
    >>$
    nfsub:=length fsub$  % must be >1 for potential-integration
    h:=intminderiv(p,ftem,vlrev,maxvanz,nfsub)$ % fsub is also for below
    intlist:=car h$
    %-- list of necessary integrations of the whole equation to solve
    %-- for a function of all variables
    listoflds:=cadr h$ %-- list of leading derivatives
  >>$
  if pri then <<terpri()$
                write"complete integrations:",intlist," for:",vl>>;
  %-- n1 is the number of integrations which must be done to try
  %-- potential integration which must enable to eliminate 2 functions
  %-- n2 is the number of integrations actually done
  n1:=for each h in intlist sum h;
  if (not vari) and (zerop n1) then
  <<n2:=0;
    if potflag then % else not necessary
    for h:=1:(length vl) do ifac0:=cons(nil,ifac0)
  >>                           else
  <<if tr_genint then
    <<terpri()$write "integration of the expression : "$
      eqprint p>>$
    if pri then
    <<terpri()$write"at first all multiple complete integration">>;
    %-- At first if possible n2 integrations of the whole equation
    h:=multipleint(intlist,ftem,p,vari,vl,genflag,nil,nil,nil)$
                   % potflag=nil, partial=nil, doneintvar=nil
    if h then
    <<p:=car h;
      ifac0:=cadr h;  % ifac0 is the list of lists of integr. factors
      newcond:=caddr h;
      n2:=cadddr h;   % number of done integrations
      % doneintvar & intfacdep for the two halfs of integrations
      % from the two parts of ifac0
      h:=nil;
      iflag:='success;
    >>   else n2:=0;
    ftem:=union(fnew_,ftem)$
  >>;
  %------------ Existence of a potential ?
  if (n1=n2) and potflag and (nfsub>1) then
  %---- at least 2 functions to solve for
  <<if not zerop n2 then            %---- update listoflds
    listoflds:=uplistoflds(reverse ifac0,listoflds)$
    if pri then <<terpri()$write"uplistoflds:",listoflds>>$
    if h:=potintegrable(listoflds) then
    <<ili1a:=car h; ili2a:=cadr h;
      % The necess. differentiations of the potential
      if pri then
      <<terpri()$write"potintegrable:",ili1a,"  ",ili2a>>$

      if pri then <<write"+++ intlist=",intlist,
                           "    ili1a=",ili1a,
                           "    ili2a=",ili2a>>$
      %-- distributing the integrating factors of ifac0 among
      %-- the two lists ifac1b and ifac2b which are so far nil
      %-- such that (ifac1b and ili1a are disjunct) and
      %--           (ifac2b and ili2a are disjunct)
      v1a:=vlofintlist(vl,ili1a);
      v2a:=vlofintlist(vl,ili2a);

      hh:=t;
      cop:=reverse ifac0;
      ifac1a:=ili1a;ifac2a:=ili2a;
      while hh and cop do <<
        % cop is a list of lists of integr. factors
        if car cop then h:=vlofintfaclist(vl,cdar cop)
                   else h:=nil;
        if freeoflist(h,v2a) and (car ifac2a=0) then <<
          ifac1b:=cons( nil, ifac1b);
          ifac2b:=cons( reverse car cop, ifac2b)
        >>                   else
        if freeoflist(h,v1a) and (car ifac1a=0) then <<
          ifac2b:=cons( nil, ifac2b);
          ifac1b:=cons( reverse car cop, ifac1b)
        >>                   else
        if car cop then hh:=nil;
        ifac1a:=cdr ifac1a;
        ifac2a:=cdr ifac2a;
        cop:=cdr cop;
      >>;
      % the elements of ifac1b,ifac2b are in reverse order to
      % ifac1a,ifac2a and are in the same order as vl, also
      % the elements in the infac-lists are in inverse order,
      % i.e. in the order the integrations have been done
      if pri then <<terpri()$
                    write  "ifac1a=",ifac1a,"  ifac1b=",ifac1b,
                    "  ifac2a=",ifac2a,"  ifac2b=",ifac2b >>$

      %-- lists of integrations to be done to both parts
      if hh then
      repeat % possibly a second try with part2 integrated first
      <<n1:=for each n1 in ili1a sum n1;
        % n1 .. number of remaining integrations of the first half
        p1:=multipleint(ili1a,ftem,p,nil,vl,genflag,t,t,
                        % potflag=t, partial=t
                        union(vlofintlist(vl,ili2a),
                              vlofintlist(vl,ifac1b)))$
        % that the variables of integration are not in ifac1b
        % was already checked. Only restriction: the integrating
        % factors must not depend on the last argument.

        ftem:=union(fnew_,ftem)$
        if p1 then <<
          ifac1a:=cadr p1;
          % ifac1a is now the list of integrating factors
          if newcond then newcond:=nconc(newcond,caddr p1)
                     else newcond:=caddr p1;
          if pri then <<terpri()$write"mul2: newcond=",newcond>>$
          n2:=cadddr p1;
          p1:=car p1
        >>;
        if p1 and (n1=n2) then
        %--- if the first half has been integrated suff. often
        <<%--- integrating the second half sufficiently often
          n1:=for each n1 in ili2a sum n1;
          % calculation of the 2. part which is not contained in p1
          p2:=p1;
          cop:=ifac1a; hh:=vlrev; % because ifac1a is reversed
          while cop do <<
            h:=car cop;cop:=cdr cop;
            v:=car hh;hh:=cdr hh;
            % h is the list of integrating factors of the v-integr.
            while h do <<
              p2:=reval list('QUOTIENT,list('DF,p2,v),car h);
              h:=cdr h
            >>
          >>;
          p2:=reval reval list('PLUS,p,list('MINUS,p2));
          p2:=multipleint(ili2a,ftem,p2,nil,vl,genflag,t,nil,
                          % potflag=t, partial=nil
                          union(vlofintlist(vl,ili1a),
                                vlofintlist(vl,ifac2b)))$
          ftem:=union(fnew_,ftem)$
          if p2 then <<
            ifac2a:=cadr p2;
            % ifac2a is now list of integrating factors
            if newcond then newcond:=nconc(newcond,caddr p2)
                       else newcond:=caddr p2;
            if pri then <<terpri()$write"mul3: newcond=",newcond>>$
            n2:=cadddr p2;
            p2:=car p2
          >>;
          if p2 and (n1=n2) then
          % if the second half has been integrated sufficiently often
          <<% can both halfes be solved for different functions
            % i.e. are inhomogeneous
  % part which is of advantage for finding integrating factors
  q:=splitinhom(q,ftem,vl)$
  qh:=car q; qih:=cdr q; q:=nil;

  while (vari or vlcop) and (pih or (not potflag)) do
  %------- if for potflag=t one variable can not be integrated the
  %------- maximal number of times (nmax) then immediate stop because
  %------- then no elimination of two functions will be possible
  << %-- The next integration variable: v, no of integrations: nmax
    if vari then <<v:=vari;nmax:=1>>
            else <<v:=car vlcop;     vlcop:=cdr vlcop;
                   nmax:=car intvar; intvar:=cdr intvar>>;

    if zerop nmax then intlist:=cons(nil,intlist)
                  else
    <<if pri then write"anf: intvar=",intvar," vari=",vari,"    q=",q$
      if vari and (not member(v,vl)) then
      <<qh :=reval list('INT,qh ,v)$
        if freeof(qh,'INT) then <<
          qih:=reval list('INT,qih,v)$
          iflag:=if freeint_ and
                    (null freeof(qih,'INT)) then nil else
                 if freeabs_ and
                    (null freeof(qih,'ABS)) then nil else <<
                   intlist:=cons(list(1),intlist)$
                   'success>>$
          if pri then <<write"232323 qh=",qh;terpri();
                        write"qih=",qih;terpri()>>
        >>
      >>                             else
      <<n:=0$
        if pri then write"333"$
        intfaclist:=nil; %-- the list of integr. factors in v-integr.
        if potflag or my_freeof(intfacdep,v) then
        % otherwise v-integration not allowed because one integrating
        % factor already depends on v
        % for potflag=t this `commutativity'-demand plays no role
        repeat << %--- max nmax integrations of qh and qih wrt. v
          if pri then <<write"444  vor intpde:"$eqprint q$terpri()$
                        write"potflag=",potflag," v=",v,
                        "  ftem=",ftem>>$
          % At first trying a direct integration of the homog. part qh
          ph:=intpde(qh,ftem,vl,v,partial)$  % faster if potflag=nil
          if pri then <<write"nach intpde(qh):"$deprint ph>>$

          %------ At first the integration of the homogeneous part
          intfacdepnew:=intfacdep;
          if ph and (partial or (zerop cadr ph)) then <<
            %---- For the homogen. part cadr ph must be zero
            intfaclist:=cons(1,intfaclist);
            ph:=car ph;
            if pri then <<write"565656 ph=",ph;terpri()>>;
          >>                                     else
          if vari then ph:=nil
                  else
          if facint_ then <<
            ph:=findintfac(list(qh),ftem,vl,v,doneintvar,intfacdep,
                           not zerop n,not potflag);
            % factorize before ivestig., no report of int. factors
            if ph then << %--- Complete integr. of qh was possible
              if pri then write"of the homogeneous part"$terpri()$
              %--- update the list of variables on which all integr.
              %--- factors depend apart from the integration variable
              intfacdepnew:=caddr ph;
              %--- extend the list of integrating factors, cadr ph
              %--- is a list of integr. factors, here only one
              intfaclist:=cons(caadr ph,intfaclist);
              %--- multiply the inhomogeneous part with integ. factor
              qih:=reval reval reval list('TIMES,car intfaclist,qih);
              if pri then <<write"454545 qih=",qih;terpri()>>;
              ph:=car ph  % the integral of qh
            >>
          >>;

          %------ Now the integration of the inhomogeneous part
          if not ph then pih:=nil %--- no integration possible
                    else <<
            if zerop qih then pih:=list(0,0) else
            pih:=intpde(qih,ftem,vl,v,partial)$

            if print_ and null pih then <<
              terpri()$
              write"Inhomogeneous part: "$
              mathprint qih$
              write"can not be integrated explicitly wrt. ",v$
            >>$
            if pri then <<write"nach intpde(qih):",pih$terpri()$
                          write"genflag=",genflag$terpri()>>$

            if pih then
            if zerop cadr pih then
            <<qih:=car pih$n:=add1 n$iflag:='success$
              if pri then write" success "$
            >>
                              else if not genflag then pih:=nil
                                                  else
            <<if pri then write"555"$
              geni:=partint(cadr pih,smemberl(ftem,cadr pih),
                            vl,v,genflag)$
              if geni then
              <<qih:=reval list('PLUS,car pih,car geni)$
                n:=add1 n$
                ftem:=union(fnew_,ftem)$
                newcond:=append(cdr geni,newcond)$  % additional de's
                if pri then
                <<terpri()$write"nach gen newcond:",newcond>>$
                iflag:='genint
              >>                       else
              if partial then qih:=car pih
                         else pih:=nil
            >>;
            if pih then <<
              if pri then write"AAA"$
               qh:=ph;
              if (not potflag) and (n neq 1) then
              intfacdep:=intfacdepnew
              %-The first integr. factor of all v-integrations does not
              % depend on any earlier integration variables and can
              % therefore be taken out of all integrals --> no incease
              % of intfacdep for n=1.
              %-For potential integration the integration variables and
              % extra-dependencies-variables of integr. factors need not
              % be disjunct therefore the intfacdep-update only for
              % not-potflag
            >>     else <<
              if pri then write"BBB"$
              % inhomogeneous part can not be integrated therefore
              % reversing the succesful integration of the hom. part
              if car intfaclist neq 1 then
              qih:=reval list('QUOTIENT,qih,car intfaclist);
              intfaclist:=cdr intfaclist
            >>;
          >>; %-- end of the integration of the inhomog. part
          if pri then write"n=",n," nmax=",nmax," not pih=",not pih$
        >> until (n=nmax) or (not pih)$ %---- end of v-integration
        %------- variables of done integrations are collected for
        %------- determining integrating factors in later integr.
        if not zerop n then doneintvar:=cons(v,doneintvar)$
        nges:=nges+n;
        intlist:=cons(intfaclist,intlist)
      >>$  % of    not ( vari and (not member(v,vl)))
      if vari then <<vari:=nil;vlcop:=nil>>;
      if pri then write"ende: intvar=",intvar," vari=",vari,
      "    qh=",qh,"   qih=",qih$
    >> % of (nmax neq zero)
  >>$  % of ( while (vari or vlcop) and (pih or (not potflag)) )

  % intlist and its elements intfaclist are in the inverse order
  % to vl and the sequence of integrations done
  q:=reval list('PLUS,qh,qih)$ %--- adding homog. and inhomog. part
  if pri then <<terpri()$write"\\\  newcond:"$deprint newcond;
    write"multiple result:",if null iflag then nil
    else list(q,intlist,newcond,nges)
  >>;
  return if null iflag then nil
                       else list(q,intlist,newcond,nges)
end$  % of multipleint

symbolic procedure uplistoflds(intlist,listoflds)$
begin
  scalar f,h1,h2,h3,h4,lds,itl;
  while listoflds do
  <<f:=caar listoflds;
    lds:=cdar listoflds;
    listoflds:=cdr listoflds;
    h2:=nil;            % h2 becomes the new list of lds of f
    while lds do
    <<h3:=car lds; lds:=cdr lds;
      itl:=intlist;
      h4:=nil;          % h4 becomes one new ld of f
      while h3 do
      <<h4:=cons(car h3 - if null car itl then 0
                                          else length car itl, h4);
        h3:=cdr h3;itl:=cdr itl
      >>;
      h2:=cons(reverse h4,h2)
    >>;
    h1:=cons(cons(f,h2),h1)
  >>;
  return h1  % updated listoflds
end$ % of uplistoflds

symbolic procedure addintco(q, ftem, ifac, vl, vari)$
begin scalar v,f,l,vl1;
  % multi.ing factors to the constants/functions of integration
  if zerop q then l:=1
             else
  <<ftem:=fctsort ftem$
    while ftem do
    if fctlength car ftem<length vl then ftem:=nil
                                    else if fctlinear(q,f)          then
                                         <<f:=car ftem$ ftem:=nil>> else
                                         ftem:=cdr ftem$
    if f then
    <<l:=lderiv(q,f,fctargs f)$
      l:=reval coeffn(q,reval car l,cdr l)
    >>   else l:=1
  >>;
  % the constants and functions of integration
  if vari then q:=list('PLUS,q,intconst(l,vl,vari,list(1)))
          else
  <<vl1:=vl;
    while vl1 do
    <<v:=car vl1; vl1:=cdr vl1;
      if car ifac then
      q:=list('PLUS,q,intconst(l,vl,v,car ifac))$
      % l..product of factors in the coefficient of the function to be
      % eliminated, car ifac .. list of integrating factors
      ifac:=cdr ifac;
    >>
  >>$
  return reval q
end$ % of addintco

symbolic procedure integratepde(p,ftem,vari,genflag,potflag)$
%  Generalized integration of the expression p
%     if not genflag then "normal integration"
%  Equation p must not be directly separable, otherwise the depen-
%  dencies of functions of integration on their variables is wrong,
%  i.e. no dependence on explicit variables
%  ftem are all functions from the `global' ftem which occur in p, i.e.
%  ftem:=smemberl(ftem,p)$
%  if vari=nil then integration w.r.t. all possible variables within
%                   the equation
%              else only w.r.t. vari one time

begin
  scalar vl,vlrev,v,intlist,
  ili1a,ili2a,maxvanz,fsub,h,hh,nfsub,iflag,newcond,
  n1,n2,pot1,pot2,p1,p2,listoflds,secnd,ifac0,
  ifac1a,ifac1b,ifac2a,ifac2b,cop,v1a,v2a,pri$

  % pri:=t;
  if pri then <<terpri()$write"Start Integratepde">>$
  vl:=argset ftem$
  vlrev:=reverse vl;
  if vari then <<potflag:=nil;
                 if zerop p then iflag:='success>>
          else
  <<%------- determining fsub=list of functions of all variables
    maxvanz:=length vl$
    fsub:=nil;
    h:=ftem;
    while h do
    <<if fctlength car h=maxvanz then
      fsub:=cons(car h,fsub)$
      h:=cdr h
    >>$
    nfsub:=length fsub$  % must be >1 for potential-integration
    h:=intminderiv(p,ftem,vlrev,maxvanz,nfsub)$ % fsub is also for below
    intlist:=car h$
    %-- list of necessary integrations of the whole equation to solve
    %-- for a function of all variables
    listoflds:=cadr h$ %-- list of leading derivatives
  >>$
  if pri then <<terpri()$
                write"complete integrations:",intlist," for:",vl>>;
  %-- n1 is the number of integrations which must be done to try
  %-- potential integration which must enable to eliminate 2 functions
  %-- n2 is the number of integrations actually done
  n1:=for each h in intlist sum h;
  if (not vari) and (zerop n1) then
  <<n2:=0;
    if potflag then % else not necessary
    for h:=1:(length vl) do ifac0:=cons(nil,ifac0)
  >>                           else
  <<if tr_genint then
    <<terpri()$write "integration of the expression : "$
      eqprint p>>$
    if pri then
    <<terpri()$write"at first all multiple complete integration">>;
    %-- At first if possible n2 integrations of the whole equation
    h:=multipleint(intlist,ftem,p,vari,vl,genflag,nil,nil,nil)$
                   % potflag=nil, partial=nil, doneintvar=nil
    if h then
    <<p:=car h;
      ifac0:=cadr h;  % ifac0 is the list of lists of integr. factors
      newcond:=caddr h;
      n2:=cadddr h;   % number of done integrations
      % doneintvar & intfacdep for the two halfs of integrations
      % from the two parts of ifac0
      h:=nil;
      iflag:='success;
    >>   else n2:=0;
    ftem:=union(fnew_,ftem)$
  >>;
  %------------ Existence of a potential ?
  if (n1=n2) and potflag and (nfsub>1) then
  %---- at least 2 functions to solve for
  <<if not zerop n2 then            %---- update listoflds
    listoflds:=uplistoflds(reverse ifac0,listoflds)$
    if pri then <<terpri()$write"uplistoflds:",listoflds>>$
    if h:=potintegrable(listoflds) then
    <<ili1a:=car h; ili2a:=cadr h;
      % The necess. differentiations of the potential
      if pri then
      <<terpri()$write"potintegrable:",ili1a,"  ",ili2a>>$

      if pri then <<write"+++ intlist=",intlist,
                           "    ili1a=",ili1a,
                           "    ili2a=",ili2a>>$
      %-- distributing the integrating factors of ifac0 among
      %-- the two lists ifac1b and ifac2b which are so far nil
      %-- such that (ifac1b and ili1a are disjunct) and
      %--           (ifac2b and ili2a are disjunct)
      v1a:=vlofintlist(vl,ili1a);
      v2a:=vlofintlist(vl,ili2a);

      hh:=t;
      cop:=reverse ifac0;
      ifac1a:=ili1a;ifac2a:=ili2a;
      while hh and cop do <<
        % cop is a list of lists of integr. factors
        if car cop then h:=vlofintfaclist(vl,cdar cop)
                   else h:=nil;
        if freeoflist(h,v2a) and (car ifac2a=0) then <<
          ifac1b:=cons( nil, ifac1b);
          ifac2b:=cons( reverse car cop, ifac2b)
        >>                   else
        if freeoflist(h,v1a) and (car ifac1a=0) then <<
          ifac2b:=cons( nil, ifac2b);
          ifac1b:=cons( reverse car cop, ifac1b)
        >>                   else
        if car cop then hh:=nil;
        ifac1a:=cdr ifac1a;
        ifac2a:=cdr ifac2a;
        cop:=cdr cop;
      >>;
      % the elements of ifac1b,ifac2b are in reverse order to
      % ifac1a,ifac2a and are in the same order as vl, also
      % the elements in the infac-lists are in inverse order,
      % i.e. in the order the integrations have been done
      if pri then <<terpri()$
                    write  "ifac1a=",ifac1a,"  ifac1b=",ifac1b,
                    "  ifac2a=",ifac2a,"  ifac2b=",ifac2b >>$

      %-- lists of integrations to be done to both parts
      if hh then
      repeat % possibly a second try with part2 integrated first
      <<n1:=for each n1 in ili1a sum n1;
        % n1 .. number of remaining integrations of the first half
        p1:=multipleint(ili1a,ftem,p,nil,vl,genflag,t,t,
                        % potflag=t, partial=t
                        union(vlofintlist(vl,ili2a),
                              vlofintlist(vl,ifac1b)))$
        % that the variables of integration are not in ifac1b
        % was already checked. Only restriction: the integrating
        % factors must not depend on the last argument.

        ftem:=union(fnew_,ftem)$
        if p1 then <<
          ifac1a:=cadr p1;
          % ifac1a is now the list of integrating factors
          if newcond then newcond:=nconc(newcond,caddr p1)
                     else newcond:=caddr p1;
          if pri then <<terpri()$write"mul2: newcond=",newcond>>$
          n2:=cadddr p1;
          p1:=car p1
        >>;
        if p1 and (n1=n2) then
        %--- if the first half has been integrated suff. often
        <<%--- integrating the second half sufficiently often
          n1:=for each n1 in ili2a sum n1;
          % calculation of the 2. part which is not contained in p1
          p2:=p1;
          cop:=ifac1a; hh:=vlrev; % because ifac1a is reversed
          while cop do <<
            h:=car cop;cop:=cdr cop;
            v:=car hh;hh:=cdr hh;
            % h is the list of integrating factors of the v-integr.
            while h do <<
              p2:=reval list('QUOTIENT,list('DF,p2,v),car h);
              h:=cdr h
            >>
          >>;
          p2:=reval reval list('PLUS,p,list('MINUS,p2));
          p2:=multipleint(ili2a,ftem,p2,nil,vl,genflag,t,nil,
                          % potflag=t, partial=nil
                          union(vlofintlist(vl,ili1a),
                                vlofintlist(vl,ifac2b)))$
          ftem:=union(fnew_,ftem)$
          if p2 then <<
            ifac2a:=cadr p2;
            % ifac2a is now list of integrating factors
            if newcond then newcond:=nconc(newcond,caddr p2)
                       else newcond:=caddr p2;
            if pri then <<terpri()$write"mul3: newcond=",newcond>>$
            n2:=cadddr p2;
            p2:=car p2
          >>;
          if p2 and (n1=n2) then
          % if the second half has been integrated sufficiently often
          <<% can both halfes be solved for different functions
            % i.e. are inhomogeneous
  % part which is of advantage for finding integrating factors
  q:=splitinhom(q,ftem,vl)$
  qh:=car q; qih:=cdr q; q:=nil;

  while (vari or vlcop) and (pih or (not potflag)) do
  %------- if for potflag=t one variable can not be integrated the
  %------- maximal number of times (nmax) then immediate stop because
  %------- then no elimination of two functions will be possible
  << %-- The next integration variable: v, no of integrations: nmax
    if vari then <<v:=vari;nmax:=1>>
            else <<v:=car vlcop;     vlcop:=cdr vlcop;
                   nmax:=car intvar; intvar:=cdr intvar>>;

    if zerop nmax then intlist:=cons(nil,intlist)
                  else
    <<if pri then write"anf: intvar=",intvar," vari=",vari,"    q=",q$
      if vari and (not member(v,vl)) then
      <<qh :=reval list('INT,qh ,v)$
        if freeof(qh,'INT) then <<
          qih:=reval list('INT,qih,v)$
          iflag:=if freeint_ and
                    (null freeof(qih,'INT)) then nil else
                 if freeabs_ and
                    (null freeof(qih,'ABS)) then nil else <<
                   intlist:=cons(list(1),intlist)$
                   'success>>$
          if pri then <<write"232323 qh=",qh;terpri();
                        write"qih=",qih;terpri()>>
        >>
      >>                             else
      <<n:=0$
        if pri then write"333"$
        intfaclist:=nil; %-- the list of integr. factors in v-integr.
        if potflag or my_freeof(intfacdep,v) then
        % otherwise v-integration not allowed because one integrating
        % factor already depends on v
        % for potflag=t this `commutativity'-demand plays no role
        repeat << %--- max nmax integrations of qh and qih wrt. v
          if pri then <<write"444  vor intpde:"$eqprint q$terpri()$
                        write"potflag=",potflag," v=",v,
                        "  ftem=",ftem>>$
          % At first trying a direct integration of the homog. part qh
          ph:=intpde(qh,ftem,vl,v,partial)$  % faster if potflag=nil
          if pri then <<write"nach intpde(qh):"$deprint ph>>$

          %------ At first the integration of the homogeneous part
          intfacdepnew:=intfacdep;
          if ph and (partial or (zerop cadr ph)) then <<
            %---- For the homogen. part cadr ph must be zero
            intfaclist:=cons(1,intfaclist);
            ph:=car ph;
            if pri then <<write"565656 ph=",ph;terpri()>>;
          >>                                     else
          if vari then ph:=nil
                  else
          if facint_ then <<
            ph:=findintfac(list(qh),ftem,vl,v,doneintvar,intfacdep,
                           not zerop n,not potflag);
            % factorize before ivestig., no report of int. factors
            if ph then << %--- Complete integr. of qh was possible
              if pri then write"of the homogeneous part"$terpri()$
              %--- update the list of variables on which all integr.
              %--- factors depend apart from the integration variable
              intfacdepnew:=caddr ph;
              %--- extend the list of integrating factors, cadr ph
              %--- is a list of integr. factors, here only one
              intfaclist:=cons(caadr ph,intfaclist);
              %--- multiply the inhomogeneous part with integ. factor
              qih:=reval reval reval list('TIMES,car intfaclist,qih);
              if pri then <<write"454545 qih=",qih;terpri()>>;
              ph:=car ph  % the integral of qh
            >>
          >>;

          %------ Now the integration of the inhomogeneous part
          if not ph then pih:=nil %--- no integration possible
                    else <<
          