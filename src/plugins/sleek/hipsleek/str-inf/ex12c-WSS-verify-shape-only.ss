/*
in prelude.ss
data char_star {
  int val;
  char_star next;
}
*/

WSS<p> ==
  self::WFSeg<q>*q::char_star<0,p> 
  inv self!=null;

WFSeg<p> ==
  self=p 
  or self::char_star<v,q>*q::WFSeg<p> & v!=0
  inv true;

BADS<> ==
  self::char_star<v,q>*q::BADS<> 
  inv true;

void while1(ref char_star s)
  requires s::WSS<p> 
  ensures s::WFSeg<s'>*s'::char_star<0,p> ;
{
  int x=get_char(s);
  if (x!=0) {
    s = plus_plus_char(s);
    while1(s);
  }
}


void while2(ref char_star s1,ref char_star s2)
  requires s1::char_star<_,q>*q::BADS<> * s2::WSS<p>@L 
  ensures s1::WFSeg<s1a>*s1a::char_star<0,s1'>*s1'::BADS<>;
{
  int x=get_char(s2);
  write_char(s1,x);
  s2 = plus_plus_char(s2);
  s1 = plus_plus_char(s1);
  if (x!=0) {
    while2(s1,s2);
  }
}

/*
# ex12c.ss

Using segment:




*/
