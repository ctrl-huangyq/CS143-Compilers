/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

#include <stack>

unsigned short str_len;
unsigned short comment_count = 0;

bool too_long() {
    if(str_len >= 1024) {
        cool_yylval.error_msg = "String constant too long";
        return true;
    }
    return false;
}
%}

%option noyywrap

/*
 * Define names for regular expressions here.
 */

DARROW          =>
NEWLINE         \n
SPACE           [\ \f\r\t\v]*
CLASS           (?i:class)
ELSE            (?i:else)
FI              (?i:fi)
IF              (?i:if)
IN              (?i:in)
INHERITS        (?i:inherits)
ISVOID          (?i:isvoid)
LET             (?i:let)
LOOP            (?i:loop)
POOL            (?i:pool)
THEN            (?i:then)
WHILE           (?i:while)
CASE            (?i:case)
ESAC            (?i:esac)
NEW             (?i:new)
OF              (?i:of)
NOT             (?i:not)
TRUE            t(?i:rue)
FALSE           f(?i:alse)
TYPEID          [A-Z][A-Za-z0-9_]*
OBJECTID        [a-z][A-Za-z0-9_]*
INT_CONST       [0-9]*
SINGLE_CHAR     [\+\-\*\/\<\=\.\~\,\;\:\(\)\@\{\}]

%x COMMENT
%x STRING
%x STRING_ESCAPE
%x STRING_ERROR

%%

 /*
  *  Nested comments
  */
"(*"                    { 
                            BEGIN(COMMENT); 
                            comment_count++;
                        }
<COMMENT>"(*"           { comment_count++; }
<COMMENT>"*)"           { 
                            if(comment_count == 1) {
                                BEGIN(INITIAL);
                            } else {
                                BEGIN(COMMENT);
                            }
                            comment_count--;
                        }
"*)"                    {
                            cool_yylval.error_msg = "Unmatched *)";
                            return (ERROR);
                        }
<COMMENT>\n             { curr_lineno++; }
<COMMENT><<EOF>>        {
                            cool_yylval.error_msg = "EOF in comment";
                            BEGIN(INITIAL);
                            return (ERROR);
                        }
<COMMENT>.              {}

--.*                    {}

 /*
  *  The multiple-character operators.
  */
{DARROW}	 	        { return (DARROW); }
{NEWLINE}               { curr_lineno++; }
{SPACE}                 {}

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
{CLASS}                 { return (CLASS); }
{ELSE}                  { return (ELSE); }
{FI}                    { return (FI); }
{IF}                    { return (IF); }
{IN}                    { return (IN); }
{INHERITS}              { return (INHERITS); }
{ISVOID}                { return (ISVOID); }
{LET}                   { return (LET); }
{LOOP}                  { return (LOOP); }
{POOL}                  { return (POOL); }
{THEN}                  { return (THEN); }
{WHILE}                 { return (WHILE); }
{CASE}                  { return (CASE); }
{ESAC}                  { return (ESAC); }
{NEW}                   { return (NEW); }
{OF}                    { return (OF); }
{NOT}                   { return (NOT); }

{TRUE}                  {
                            yylval.boolean = true;
                            return (BOOL_CONST);
                        }
{FALSE}                 {
                            yylval.boolean = false;
                            return (BOOL_CONST);
                        }
    
{TYPEID}                { 
                            yylval.symbol = idtable.add_string(yytext);
                            return (TYPEID);
                        }
{OBJECTID}              {
                            yylval.symbol = idtable.add_string(yytext);
                            return (OBJECTID);
                        }
{INT_CONST}             {
                            cool_yylval.symbol = inttable.add_string(yytext, yyleng);
                            return (INT_CONST);
                        }

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */
\"                      {
                            BEGIN(STRING);
                            str_len = 0;
                        }
<STRING>\\              {
                            BEGIN(STRING_ESCAPE);
                        }
<STRING_ESCAPE>n        {
                            if(too_long()) {
                                BEGIN(STRING_ERROR);
                                cool_yylval.error_msg = "String constant too long";
                                return (ERROR);
                            }
                            string_buf[str_len++] = '\n';
                            BEGIN(STRING);
                        }
<STRING_ESCAPE>b        {
                            if(too_long()) {
                                BEGIN(STRING_ERROR);
                                cool_yylval.error_msg = "String constant too long";
                                return (ERROR);
                            }
                            string_buf[str_len++] = '\b';
                            BEGIN(STRING);
                        }
<STRING_ESCAPE>t        {
                            if(too_long()) {
                                BEGIN(STRING_ERROR);
                                cool_yylval.error_msg = "String constant too long";
                                return (ERROR);
                            }
                            string_buf[str_len++] = '\t';
                            BEGIN(STRING);
                        }
<STRING_ESCAPE>f        {
                            if(too_long()) {
                                BEGIN(STRING_ERROR);
                                cool_yylval.error_msg = "String constant too long";
                                return (ERROR);
                            }
                            string_buf[str_len++] = '\f';
                            BEGIN(STRING);
                        }
<STRING_ESCAPE>\n       {
                            if(too_long()) {
                                BEGIN(STRING_ERROR);
                                cool_yylval.error_msg = "String constant too long";
                                return (ERROR);
                            }
                            string_buf[str_len++] = '\n';
                            curr_lineno++;
                            BEGIN(STRING);
                        }
<STRING_ESCAPE>\0       {
                            cool_yylval.error_msg = "String contains null character";
                            BEGIN(STRING_ERROR);
                            return (ERROR);
                        }
<STRING_ESCAPE>.        {
                            if(too_long()) {
                                BEGIN(STRING_ERROR);
                                cool_yylval.error_msg = "String constant too long";
                                return (ERROR);
                            }
                            string_buf[str_len++] = yytext[0];
                            BEGIN(STRING);
                        }
<STRING_ESCAPE><<EOF>>  {
                            cool_yylval.error_msg = "EOF in string constant";
                            BEGIN(INITIAL);
                            return (ERROR);
                        }
<STRING><<EOF>>         {
                            cool_yylval.error_msg = "EOF in string constant";
                            BEGIN(INITIAL);
                            return (ERROR);
                        }
<STRING>\"              {
                            string_buf[str_len] = '\0';
                            yylval.symbol = stringtable.add_string(string_buf);
                            BEGIN(INITIAL);
                            return (STR_CONST);
                        }
<STRING>\n              {
                            cool_yylval.error_msg = "Unterminated string constant";
                            curr_lineno++;
                            BEGIN(INITIAL);
                            return (ERROR);
                        }
<STRING>\0              {
                            cool_yylval.error_msg = "String contains null character";
                            BEGIN(STRING_ERROR);
                            return (ERROR);
                        }
<STRING>.               {
                            if(too_long()) {
                                cool_yylval.error_msg = "String constant too long";
                                BEGIN(STRING_ERROR);
                                return (ERROR);
                            }
                            string_buf[str_len++] = yytext[0];
                        }
<STRING_ERROR>\n        {
                            curr_lineno++;
                            BEGIN(INITIAL);
                        }
<STRING_ERROR>\"        { BEGIN(INITIAL); }
<STRING_ERROR>.         {}


"<="                    { return (LE); }
"<-"                    { return (ASSIGN);}
{SINGLE_CHAR}           { return yytext[0]; }
.                       {
                            cool_yylval.error_msg = yytext;
                            return (ERROR);
                        }
%%
