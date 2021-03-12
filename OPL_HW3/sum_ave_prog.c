#include <stdio.h>
#include <stdlib.h>

int getint() {
// returns an integer from standard input or
// prints an appropriate error message and dies.
	int value;
	scanf("%d", &value); // add error checking for EOF or non-integer input
	
	return value;
}

void putint(int n) {
// prints an integer and a linefeed to standard output.
	printf("%d\n", n);
}

int main (void) {
	/* everything above this line is the program prologue */

	/* Calculator program translation starts here */
	int a;
	int b;
	int sum;
	a = getint();  /* read a */
	b = getint();  /* read b */
	sum = a + b; /* sum := a + b */
	putint(sum);          /* write sum */
	putint(sum/2);        /* write sum/2 */
	/* Calculator program translation ends here */
	
	/* everything below this line is the program epilogue */
	return 0;
}