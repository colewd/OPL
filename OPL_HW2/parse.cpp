/* Complete recursive descent parser for the calculator language.
    Builds on figure 2.16.  Prints a trace of productions predicted and
    tokens matched.  Does no error recovery: prints "syntax error" and
    dies on invalid input.
    Michael L. Scott, 2008-2017.
*/

#include "stdio.h"
#include "stdlib.h"
#include<iostream>
#include<vector>
#include<exception>

#include "scan.h"

const char* names[] = {"read", "write", "id", "literal", "gets",
                       "add", "sub", "mul", "div", "lparen", "rparen", 
				"eof", "if", "fi", "do","od", "check", ">", "<", ">=",
				 "<=", "==", "<>"};

static token input_token;

class myex: public std::exception
{
	public:
		token expected;
		token got;
		myex(token expected, token got) {
			this->expected = expected;
			this->got = got;
			this->what();
		}
  		virtual const char* what() const throw()
  		{
    			std::cout << "Expected: " << names[expected] << " got: " << names[got] << std::endl;
  		}
}; 		

void match (token expected) {
    if (input_token == expected) {
        /*std::cout << "matched " << names[input_token] << std::endl;
        if (input_token == t_id || input_token == t_literal)
            std::cout<< ": " << token_image << std::endl;*/
        input_token = scan ();
    }
    else{
		std::cout << "Syntax Error! Incorrect Token\n";
		throw myex(expected, input_token);
		input_token = expected;
	}
}

void program ();
void stmt_list ();
void stmt ();
void expr ();
void term_tail ();
void term ();
void factor_tail ();
void factor ();
void add_op ();
void mul_op ();
void ro_op ();
void relation ();
void expr_tail ();

void program () {
 try{
    switch (input_token) {
	case t_id:
	case t_read:
	case t_write:
	case if_id:
	case check_id:
	case do_id:
	case t_eof:
		std::cout << "(program \n [";
		stmt_list ();
           match (t_eof);
		std::cout << "]\n)" << std::endl;
           break;
	default:
		std::cout << "error in program(), got " << names[input_token] << "\n";
		throw myex(t_id, input_token);
    }
 }catch(myex e){
	while (true) {
            switch (input_token) {
                case if_id:
                case check_id:
                case do_id:
                case t_id:
                case t_read:
                case t_write:
                case t_eof:
                    program ();
                    return;
                default:
			    input_token = scan();
            }
        }

 }
}

void stmt_list () {
try {
    switch (input_token) {
        case t_id:
        case t_read:
        case t_write:
	case do_id:
	case check_id:
	case if_id:
            stmt ();
            stmt_list ();
            break;
        case t_eof:
	case od_id:
	case fi_id:
            break;          
        default:
		std::cout << "error in stmt_list(), got " << names[input_token] << "\n";
		throw myex(t_id, input_token);
    }
} catch(myex e) {
	while (true) {
            switch (input_token) {
                case do_id:
                case check_id:
                case if_id:
                case t_id:
                case t_read:
                case t_write:
                case od_id:
			case t_eof:
			case fi_id:
                    stmt_list();
                    return;
                default:
			    input_token = scan();
            }
        }



}
}

void stmt () {
try{
    switch (input_token) {
        case t_id:
            std::cout<< "(:= \"" << token_image << "\" ";
            match (t_id);
            match (t_gets);
            expr ();
		std::cout << ")" << std::endl;
            break;
        case t_read:
	    std::cout << "(read \"";
            match (t_read);
		std::cout << token_image << "\")" << std::endl;
            match (t_id);
            break;
        case t_write:
	    std::cout << "(write ";
            match (t_write);
            expr ();
		std::cout << ")" << std::endl;
            break;
	case if_id:
            std::cout << "(if " << std::endl;
            match (if_id);
		std::cout << "(";
            relation ();
		std::cout << ")" << std::endl << "[";
            stmt_list ();
		std::cout << "]" << std::endl;
            match (fi_id);
		std::cout << ")" << std::endl;
            break;
        case check_id:
            std::cout << "(check ";
                match (check_id);
                relation ();
 		  std::cout << ")" << std::endl;
                break;
            case do_id:
                std::cout << "(do\n ";
                match (do_id);
 			std::cout << "[";
                stmt_list ();
			std::cout << "]" <<std::endl;
                match (od_id);
			std::cout << ")" << std::endl;
                break;

        default:
		std::cout << "error in stmt(), got " << names[input_token] << "\n";
		throw myex(t_id, input_token);

    }
} catch(myex e) {
	while (true) {
            switch (input_token) {
                case do_id:
                case check_id:
                case if_id:
                case t_id:
                case t_read:
                case t_write:
                    stmt;
                    return;
                default:
			    input_token = scan();
            }
        }


}
}

void expr () {
try{
    switch (input_token) {
        case t_id:
        case t_literal:
        case t_lparen:
			//std::cout << "predict expr --> term term_tail\n";
            term ();
            term_tail ();
            break;
        default:
		std::cout << "error in expr(), got " << names[input_token] << "\n";
		throw myex(t_id, input_token);

    }
}
catch (myex e) {
	while (true) {
            switch (input_token) {
                case t_literal:
                case t_lparen:
                case t_id:
                    expr ();
                    return;
                default:
			    input_token = scan();
            }
        }

}
}

void term_tail () {
try{
    switch (input_token) {
        case t_add:
        case t_sub:
			//std::cout << "predict term_tail --> add_op term term_tail\n";
            add_op ();
            term ();
            term_tail ();
            break;
        case t_rparen:
        case t_id:
        case t_read:
        case t_write:
case t_lthan:
            case t_gthan:
            case t_lthaneq:
            case t_gthaneq:
            case t_equal:
            case t_nequal:
            case do_id:
            case od_id:
            case if_id:
            case fi_id:

        case t_eof:
            //std::cout<<"predict term_tail --> epsilon\n";
            break;          /*  epsilon production */
        default:
		std::cout << "error in term_tail(), got " << names[input_token] << "\n";
		throw myex(t_id, input_token);

    }
}
catch (myex e) {
	while (true) {
            switch (input_token) {
                case t_add:
                case t_sub:
                    term_tail ();
                    return;
                default:
			    input_token = scan();
            }
        }

}

}

void term () {
try{
    switch (input_token) {
        case t_id:
        case t_literal:
        case t_lparen:
            factor ();
            factor_tail ();
            break;
        default:
		std::cout << "error in term(), got " << names[input_token] << "\n";
		throw myex(t_id, input_token);

    }
} catch(myex e){
	while (true) {
            switch (input_token) {
                case t_id:
                case t_literal:
			case t_lparen:
			    term();
                    return;
                default:
			    input_token = scan();
            }
        }

}
}

void factor_tail () {
try{
    switch (input_token) {
        case t_mul:
        case t_div:
			//std::cout << "predict factor_tail --> mul_op factor factor_tail\n";
            mul_op ();
            factor ();
            factor_tail ();
            break;
        case t_add:
        case t_sub:
        case t_rparen:
        case t_id:
        case t_read:
        case t_write:
 case t_lthan:
            case t_gthan:
            case t_lthaneq:
            case t_gthaneq:
            case t_equal:
            case t_nequal:
            case check_id:
            case if_id:
            case fi_id:
            case do_id:
            case od_id:

        case t_eof:
			//std::cout << "predict factor_tail --> epsilon\n";
            break;          /*  epsilon production */
        default:
		std::cout << "error in factor_tail(), got " << names[input_token] << "\n";
		throw myex(t_id, input_token);

    }
}
catch(myex e){

	while (true) {
            switch (input_token) {
                case t_mul:
                case t_div:
			    factor_tail();
                    return;
                default:
			    input_token = scan();
            }
        }

}

}

void factor () {
try{
    switch (input_token) {
        case t_id :
			std::cout << "(id \"" << token_image << "\")";;
            match (t_id);
            break;
        case t_literal:
			std::cout << "(num \"" << token_image << "\")";
            match (t_literal);
            break;
        case t_lparen:
			std::cout << "(( ";

            match (t_lparen);
            expr ();
            match (t_rparen);
 			std::cout << ")) ";

            break;
        default:
		std::cout << "error in factor(), got " << names[input_token] << "\n";
		throw myex(t_id, input_token);

    }
 } catch (myex e) {
	while (true) {
            switch (input_token) {
                case t_id:
                case t_lparen:
			case t_literal:
			    factor();
                    return;
                default:
			    input_token = scan();
            }
        }


}
}

void expr_tail () {
try{
        switch (input_token) {
            case t_lthan:
            case t_gthan:
            case t_lthaneq:
            case t_gthaneq:
            case t_equal:
            case t_nequal:
                ro_op ();
                expr ();
                break;
            default:
                std::cout << "error in expr_tail(), got " << names[input_token] << "\n";
			throw myex(t_lthan, input_token);

        }
}catch (myex e) {
	while (true) {
            switch (input_token) {
                case t_gthaneq:
                case t_lthaneq:
			case t_lthan:
                case t_gthan:
                case t_equal:
			case t_nequal:

			    expr_tail();
                    return;
                default:
			    input_token = scan();
            }
        }


}

}
void add_op () {
try{
    switch (input_token) {
        case t_add:
			std::cout << "+ ";
            match (t_add);
            break;
        case t_sub:
			std::cout << "- ";

            match (t_sub);
            break;
        default:
                std::cout << "error in add_op(), got " << names[input_token] << "\n";
			throw myex(t_add, input_token);

    }
} catch (myex e) {

	while (true) {
            switch (input_token) {
                case t_add:
                case t_sub:
			    add_op();
                    return;
                default:
			    input_token = scan();
            }
        }

}

}

void mul_op () {
try{
    switch (input_token) {
        case t_mul:
			std::cout << "* ";

            match (t_mul);
            break;
        case t_div:
			std::cout << "/ ";

            match (t_div);
            break;
        default:
	           std::cout << "error in mul_op(), got " << names[input_token] << "\n";
			throw myex(t_mul, input_token);

    }
} catch (myex e) {
	while (true) {
            switch (input_token) {
                case t_mul:
                case t_div:
			    mul_op();
                    return;
                default:
			    input_token = scan();
            }
        }

}

}

void relation () {
try {
	switch(input_token) {
		case t_id:
		case t_literal:
		case t_lparen:
			expr ();
			expr_tail ();
			break;
		default:
			std::cout << "error in relation(), got " << names[input_token] << "\n";
			throw myex(t_id, input_token);
	}
} catch (myex e) {
	while (true) {
            switch (input_token) {
                case t_id:
                case t_lparen:
			case t_literal:
			    relation();
                    return;
                default:
			    input_token = scan();
            }
        }


}

}


void ro_op() {
try{

switch (input_token) {
            case t_lthan:
                std::cout << "< ";
                match (t_lthan);
                break;
            case t_gthan:
                std::cout << "> ";
                match (t_gthan);
                break;
            case t_lthaneq:
                std::cout << "<= ";
                match (t_lthaneq);
                break;
            case t_gthaneq:
                std::cout << ">= ";
                match (t_gthaneq);
                break;
            case t_equal:
                std::cout << "== ";
                match (t_equal);
                break;
            case t_nequal:
                std::cout << "<> ";
                match (t_nequal);
                break;
            default: 
               	std::cout << "error in ro_op(), got " << names[input_token] << "\n";
			throw myex(t_lthan, input_token);

        }
} catch (myex e) {
	while (true) {
            switch (input_token) {
                case t_lthan:
                case t_gthan:
                case t_lthaneq:
                case t_gthaneq:
                case t_equal:
                case t_nequal:

			    ro_op();
                    return;
                default:
			    input_token = scan();
            }
        }

}


}

int main () {
    input_token = scan ();
    program ();
    return 0;
}
