/* Simple ad-hoc scanner for the calculator language.
    Michael L. Scott, 2008-2017.
*/

#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "ctype.h"
#include <iostream>

#include "scan.h"

char token_image[100];

token scan() {
    static int c = ' ';
        /* next available char; extra (int) width accommodates EOF */
    int i = 0;              /* index into token_image */

    /* skip white space */
    while (isspace(c)) {
        c = getchar();
    }
    if (c == EOF)
        return t_eof;
    if (isalpha(c)) {
        do {
            token_image[i++] = c;
            c = getchar();
        } while (isalpha(c) || isdigit(c) || c == '_');
        token_image[i] = '\0';
        if (!strcmp(token_image, "read")) return t_read;
        else if (!strcmp(token_image, "write")) return t_write;
	else if (!strcmp(token_image, "if")) return if_id;
	else if (!strcmp(token_image, "fi")) return fi_id;
	else if (!strcmp(token_image, "do")) return do_id;
	else if (!strcmp(token_image, "od")) return od_id;
	else if (!strcmp(token_image, "check")) return check_id;
        else return t_id;
    }
    else if (isdigit(c)) {
        do {
            token_image[i++] = c;
            c = getchar();
        } while (isdigit(c));
        token_image[i] = '\0';
        return t_literal;
    } else switch (c) {
        case ':':
            if ((c = getchar()) != '=') {
		std::cerr << "error" << std::endl;
                exit(1);
            } else {
                c = getchar();
                return t_gets;
            }
            break;
        case '+': c = getchar(); return t_add;
        case '-': c = getchar(); return t_sub;
        case '*': c = getchar(); return t_mul;
        case '/': c = getchar(); return t_div;
        case '(': c = getchar(); return t_lparen;
        case ')': c = getchar(); return t_rparen;
	case '>':
            if ((c = getchar()) != '=') {
                //c = getchar(); 
                return t_gthan;
            }
            else {
                c = getchar();
                return t_gthaneq;
            }
        case '<':
            if ((c = getchar()) != '=') {
                if (c != '>') {
                    c = getchar();
                    return t_nequal;
                } else {
                    //c = getchar();
                    return t_lthan;
                }
            } else {
                c = getchar();
                return t_lthaneq;
            }
        case '=': 
            if ((c = getchar()) == '=') {
                c = getchar();
                return t_equal;
            }
            else {
                std::cerr <<  "Error: unexpected character after =\n";
                exit(1);
            }
        case '$':
            if ((c = getchar()) == '$') {
                c = getchar();
                return t_eof;
            } else {
                std::cerr << "Error: unexpected character after $\n";
            }

        default:
            printf("error\n");
            exit(1);
    }
}
