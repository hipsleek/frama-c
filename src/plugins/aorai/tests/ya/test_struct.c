/* run.config*
   STDOPT: -aorai-automata %{dep:@PTEST_DIR@/@PTEST_NAME@.ya} -aorai-acceptance
*/

struct People{
	int Age;

	char Gender;

};

struct People nobody;

int myAge=0;

void increment(){
    nobody.Age++;
    myAge++;
}


int main() {
    nobody.Age=0;
    increment();
    return 0;
}
