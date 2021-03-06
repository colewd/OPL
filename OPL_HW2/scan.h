/* Definitions the scanner shares with the parser
    Michael L. Scott, 2008-2017.
*/

typedef enum {t_read, t_write, t_id, t_literal, t_gets,
                t_add, t_sub, t_mul, t_div, t_lparen, t_rparen, t_eof,
		if_id, fi_id, do_id, od_id, check_id, t_gthan, t_lthan, 
		t_gthaneq, t_lthaneq, t_equal, t_nequal} token;

extern char token_image[100];

extern token scan();
