/*@
WFS<> ==
  self::char_star<0,q>*q::BADS<> 
  or self::char_star<v,q>*q::WFS<> & v!=0 
  inv true;

WFSeg<p> ==
  self=p 
  or self::char_star<v,q>*q::WFSeg<p> & v!=0
  inv true;

BADS<> ==
  self::char_star<v,q>*q::BADS<> 
  inv true;
*/

char *(cstrcat)(char *s1, const char *s2)
  /*@
    requires s1::WFS<> * s2::WFS<> 
    ensures s2::WFSeg<qq>*qq::char_star<0,q1>*q1::BADS<> 
      * s1::WFSeg<q3>*q3::char_star<0,q4>*q4::BADS<>;;
  */
  {
     while (*s1++!='\0') 
       /*@
          requires s1::WFS<> 
          ensures s1::WFSeg<q>*q::char_star<0,s1'>*s1'::BADS<> ;
       */
       {
         //s1++;
       }
     while ((*s1++ = *s2++) != '\0')
       /*@
          requires s1::char_star<_,q> * q::BADS<> * s2::WFS<>  
          ensures s2::WFSeg<qq>*qq::char_star<0,s2'>*s2'::BADS<> * s1'::BADS<> ;
       */
         ;   
     return s1;
  }

char* new_str()
  /*@
     requires emp
     ensures res::WFS<>;
  */
 {}

int main() 
  /*@
     requires true
     ensures res=0;
  */ 
{
  char *s1 = new_str();
  char *s2 = new_str();
  cstrcat(s1, s2);
  return 0;
}


/*=============================================================
Why this post condition cannot be derived?

Message: Post condition cannot be derived.
Procedure while_23_5$char_star~char_star FAIL.(2)
Exception Failure("Post condition cannot be derived.") Occurred!
Error(s) detected when checking procedure while_23_5$char_star~char_star


--> while (*s++!='\0'){} is different from while(*s!='\0'){s++} --> How to solve?
*/
