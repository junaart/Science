%{
#include <stdio.h>
#include <string.h>
int i=0,j;
float a[10000000];
char s[5];


%}

DIGIT    [0-9]

%%

{DIGIT}+"."{DIGIT}*" seconds of real time" {for (j = 0; j<5; j++) s[j] = yytext[j]; a[i] = atof(s); i++;}

%%
int main()
{
yylex();
int j;
FILE *f = fopen("file.txt", "w");
for (j = 0; j<=i; j++)
       {
       fprintf(f, "%f\n", a[j]);
       }
fclose(f);
       return 0;
}
	 
