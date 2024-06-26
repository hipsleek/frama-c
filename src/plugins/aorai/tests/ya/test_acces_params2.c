/* run.config*
   STDOPT: -aorai-automata %{dep:@PTEST_DIR@/@PTEST_NAME@.ya}
*/



int rr=1;
//@ global invariant inv:0<=rr<=5000;

/*@ requires r<5000;
  @ behavior j :
  @  ensures \result==r+1;
*/

int opa(int r) {return r+1;}

/*@ requires rr>=1 && rr <=5000;
  @behavior f:
  @   ensures rr>=3 && rr<=5000;
*/
void opb () {if(rr<4998) {rr+=2;}}
/*@ behavior d:
  @   ensures rr==600;
*/
void opc () {rr=600;}

/*@ requires rr==1;
  
*/
int main() {
  if (rr<5000) rr=opa(rr);
  opb();
  goto L6;
  opc();
 L6:
  return 1;


}
