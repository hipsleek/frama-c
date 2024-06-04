/* Generated by Frama-C */
struct N {
   double v ;
   int s ;
};
typedef struct N *SN;
struct L {
   int v ;
   int s ;
};
typedef struct L *SL;
struct Block {
   SN prm ;
   SN inp1 ;
   SN inp2 ;
   SN inp3 ;
   SN out1 ;
   SN out2 ;
   SN out3 ;
   SL idx1 ;
   SL idx2 ;
   SL idx3 ;
   SN sum ;
};
typedef struct Block FB;
/*@ terminates \true;
    \wp::region *fb; */
void fb_ADD(FB *fb)
{
  (fb->out1)->v += (fb->out2)->v;
  (fb->out1)->s |= (fb->out2)->s;
  return;
}

/*@ terminates \true;
    \wp::region IN: \pattern{PMEM}, (fb->inp1..fb->inp3);
    \wp::region OUT: \pattern{PVECTOR}, (fb->out1..fb->out3);
    \wp::region IDX: \pattern{PVECTOR}, (fb->idx1..fb->idx3);
 */
void fb_SORT(FB *fb)
{
  SN *inp = & fb->inp1;
  SN *out = & fb->out1;
  SL *idx = & fb->idx1;
  {
    int i = 0;
    while (i < 3) {
      (*(out + i))->v = (*(inp + i))->v + (fb->prm)->v;
      (*(out + i))->s = 0;
      (*(idx + i))->v = (*(inp + i))->s;
      (*(idx + i))->s = 0;
      i ++;
    }
  }
  (fb->sum)->v = ((fb->out1)->v + (fb->out2)->v) + (fb->out3)->v;
  (fb->sum)->s = 0;
  return;
}


