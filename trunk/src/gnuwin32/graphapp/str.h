/* Prevent multiple inclusions. */
#ifndef _STRING_CLASS_
#define _STRING_CLASS_

/* Eliminate any prior defintion of string. */
#ifdef string
#undef string
#endif

/* Define the new string type. */
#define string strclass

/* Include a few standard header files. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 *  I/O functions.
 */

#define print(s)   printf("%s",(char*)((string)s))

/*
 *  C function defintions.
 */
#ifdef __cplusplus
extern "C" {
#endif

/*
 *  Safe string utility functions.
 */
char *new_string(char *src);
void  del_string(char *str);
long  string_length(char *s);
void  copy_string(char *dest, char *src);
int   compare_strings(char *s1, char *s2);
char *add_strings(char *s1, char *s2);
char *char_to_string(char ch);
char *int_to_string(long i);
char *float_to_string(float f);

#ifdef __cplusplus
}
#endif

/*
 *  C defintions.
 */
#ifndef __cplusplus

typedef char *string;

/*
 *  C++ defintions.
 */
#else

/*
 *  String class.
 */
class string
{
  protected:
    char *str;  /* the string itself */

  public:
    string(void)        {str=new_string(NULL);}
    string(char *s)     {str=new_string(s);}
    string(string& s)   {str=new_string(s.str);}
    string(char ch)     {str=new_string(char_to_string(ch));}
    string(int i)       {str=new_string(int_to_string(i));}
    string(long i)      {str=new_string(int_to_string(i));}
    string(float f)     {str=new_string(float_to_string(f));}
    ~string(void)       {del_string(str);}

    char& operator [] (unsigned i)  {return str[i];}
    operator char* (void)           {return str;}

    string& operator = (string s2)
      {if (str != s2.str) {del_string(str); str=new_string(s2.str);} return *this;}
};

inline string operator + (string s1, string s2)
	{return add_strings(s1,s2);}
inline string operator + (string s1, char *s2)
	{return add_strings(s1,s2);}
inline string operator + (char *s1, string s2)
	{return add_strings(s1,s2);}

inline string operator + (string s1, char ch)
	{return add_strings(s1,char_to_string(ch));}
inline string operator + (char ch, string s2)
	{return add_strings(char_to_string(ch),s2);}
inline string operator + (string s1, int i)
	{return add_strings(s1,int_to_string(i));}
inline string operator + (int i, string s2)
	{return add_strings(int_to_string(i),s2);}
inline string operator + (string s1, long i)
	{return add_strings(s1,int_to_string(i));}
inline string operator + (long i, string s2)
	{return add_strings(int_to_string(i),s2);}
inline string operator + (string s1, float f)
	{return add_strings(s1,float_to_string(f));}
inline string operator + (float f, string s2)
	{return add_strings(float_to_string(f),s2);}

inline int operator == (string s1, string s2)
	{return (compare_strings(s1,s2) == 0);}
inline int operator == (string s1, char *s2)
	{return (compare_strings(s1,s2) == 0);}
inline int operator == (char *s1, string s2)
	{return (compare_strings(s1,s2) == 0);}
inline int operator == (string s1, char ch)
	{return (compare_strings(s1,char_to_string(ch)) == 0);}
inline int operator == (char ch, string s2)
	{return (compare_strings(char_to_string(ch),s2) == 0);}
inline int operator == (string s1, int ch)
	{return (compare_strings(s1,char_to_string(ch)) == 0);}
inline int operator == (int ch, string s2)
	{return (compare_strings(char_to_string(ch),s2) == 0);}

inline int operator != (string s1, string s2)
	{return compare_strings(s1,s2);}
inline int operator != (string s1, char *s2)
	{return compare_strings(s1,s2);}
inline int operator != (char *s1, string s2)
	{return compare_strings(s1,s2);}
inline int operator != (string s1, char ch)
	{return compare_strings(s1,char_to_string(ch));}
inline int operator != (char ch, string s2)
	{return compare_strings(char_to_string(ch),s2);}
inline int operator != (string s1, int ch)
	{return compare_strings(s1,char_to_string(ch));}
inline int operator != (int ch, string s2)
	{return compare_strings(char_to_string(ch),s2);}

/*
 *  End of C++ defintions.
 */
#endif

/*
 *  End of string class definition.
 */
#endif
