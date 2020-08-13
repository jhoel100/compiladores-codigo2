#include <iostream>
#include <iomanip>
#include <stack>
#include <algorithm>
#include "stdlib.h"
#include "string.h"
#include <string>

using namespace std;

enum tok_num {
    undef           =   0,  // placeholder;
    tok_eof         =   1,  // end of file
    ident           =   2,  // letter ( letter | '_' | digit )*
        rw_var      =   3,  // "var"
        rw_begin    =   4,  // "begin"
        rw_end      =   5,  // "end"
        rw_if       =   6,  // "if"
        rw_integer  =   7,  // "integer"
        rw_read     =   8,  // "read"
        rw_real     =   9,  // "real"
        rw_then     =  10,  // "then"
        rw_write    =  11,  // "write"
    lit_int         =  12,  // digit+
    lit_real        =  13,  // digit+ '.' digit+

    op_add          =  14,  // "+"
    op_sub          =  15,  // "-"
    op_mul          =  16,  // "*"
    op_div          =  17,  // "/"
    lparen          =  18,  // "("
    rparen          =  19,  // ")"
    lbrac           =  20,  // "["
    rbrac           =  21,  // "]"
    comma           =  22,  // ","
    semic           =  23,  // ";"
    colon           =  24,  // ":"
    op_lt           =  25,  // "<"
    op_gt           =  26,  // ">"
    op_le           =  27,  // "<="
    op_ge           =  28,  // ">="
    op_eq           =  29,  // "=="
    op_ne           =  30,  // "!="
    becomes         =  31,  // ":="
    tok_error       =  32   // invalid token
};

static const tok_num first_rw = rw_var;
static const tok_num last_rw  = rw_write;

struct token {
    tok_num num;
    const char* name;   
    string  image;      
    int     line;       
    int     column;     
};

extern token scan();

#include <iostream>
#include <assert.h>
#include <ctype.h>      // isspace, isdigit, isalpha, etc.
#include <string.h>
#include <stdio.h>

using namespace std;

const char *token_names[] = {
    "undef",
    "tok_eof",
    "ident",
        "rw_var",
        "rw_begin",
        "rw_end",
        "rw_if",
        "rw_integer",
        "rw_read",
        "rw_real",
        "rw_then",
        "rw_write",
    "lit_int",
    "lit_real",
    "op_add",
    "op_sub",
    "op_mul",
    "op_div",
    "lparen",
    "rparen",
    "lbrac",
    "rbrac",
    "comma",
    "semic",
    "colon",
    "op_lt",
    "op_gt",
    "op_le",
    "op_ge",
    "op_eq",
    "op_ne",
    "becomes",
    "tok_error"
};

static int cur_line = 1;              
static int cur_col  = 0;

static int tok_line;                    
static int tok_col;

static const int TOKEN_MAX = 100;
static char token_image[TOKEN_MAX];     
static int image_index;

static int cur_char = ' ';

static inline void nextchar() {
    assert(image_index < TOKEN_MAX);
    token_image[image_index++] = cur_char;
    cur_char = cin.get();
    cur_col++;
    if (cur_char == '\n') {
        cur_line++;
        cur_col = 0;
    }
}

static inline token make_token(tok_num t) {
    token rtn;
    rtn.num = t;
    assert(image_index < TOKEN_MAX);
    token_image[image_index] = 0;
    rtn.name = token_names[t];
    rtn.image = string(token_image);
    rtn.line = tok_line;
    rtn.column = tok_col;
    return rtn;
}

token scan() {
    image_index = 0;

    while (isspace(cur_char)) {
        cur_char = cin.get();
        cur_col++;
        if (cur_char == '\n') {
            cur_line++;
            cur_col = 0;
        }
    }
    tok_line = cur_line;  tok_col = cur_col;

    if (cur_char == EOF) {                     // end of file
        return make_token(tok_eof);
    }

    if (isalpha(cur_char)) {                   // identifier
        do {
            nextchar();
        } while (isdigit(cur_char) || isalpha(cur_char) || cur_char == '_');

        token_image[image_index] = 0;

        for (int w = first_rw; w <= last_rw; w++) {
            if (!strcmp(token_image, &token_names[w][3])) {
                return make_token(tok_num(w));
            }
        }
        return make_token(ident);
    }

    else if (isdigit(cur_char)) {              // literal
        tok_num tok = lit_int;
        do {
            nextchar();
        } while (isdigit(cur_char));
        if (cur_char == '.') {
            nextchar();
            if (!isdigit(cur_char)) {
                return make_token(tok_error);
            }
            tok = lit_real;
            do {
                nextchar();
            } while (isdigit(cur_char));
        }
        return make_token(tok);

    } else switch (cur_char) {                 // misc. other cases
        case ':':
            nextchar();
            if (cur_char != '=') {
                return make_token(colon);
            } else {
                nextchar();
                return make_token(becomes);
            }
            break;
        case '<':
            nextchar();
            if (cur_char != '=') {
                return make_token(op_lt);
            } else {
                nextchar();
                return make_token(op_le);
            }
            break;
        case '>':
            nextchar();
            if (cur_char != '=') {
                return make_token(op_gt);
            } else {
                nextchar();
                return make_token(op_ge);
            }
            break;
        case '=':
            nextchar();
            if (cur_char != '=') {
                return make_token(tok_error);
            } else {
                nextchar();
                return make_token(op_eq);
            }
            break;
        case '!':
            nextchar();
            if (cur_char != '=') {
                return make_token(tok_error);
            } else {
                nextchar();
                return make_token(op_ne);
            }
            break;
        case '+': nextchar(); return make_token(op_add);
        case '-': nextchar(); return make_token(op_sub);
        case '*': nextchar(); return make_token(op_mul);
        case '/': nextchar(); return make_token(op_div);
        case '(': nextchar(); return make_token(lparen);
        case ')': nextchar(); return make_token(rparen);
        case '[': nextchar(); return make_token(lbrac);
        case ']': nextchar(); return make_token(rbrac);
        case ',': nextchar(); return make_token(comma);
        case ';': nextchar(); return make_token(semic);
        default:  nextchar(); return make_token(tok_error);
    }
}

#include <iostream>
#include <vector>
#include <fstream>
#include <sstream>
#include "string.h"

using namespace std;



class Grammar{
private:
	// *** variables ***//
	int maxidx_terminal;	
	int num_nonterminals;	
	int num_rules;	
	static int start_symbol;

	// grammar input
	ifstream input; 
	string line;

	vector<string> terminal_names;  
	vector<string> nonterminal_names;
	vector<vector<int> > first_sets; 
	vector<vector<int> > follow_sets; 
	vector<vector<int> > predict_sets;  
	vector<vector<int> > parse_table; 
	vector<vector<int> > full_rules; 
	vector<vector<int> > right_hand_sides; 

private:
	
	void _grammar_counter(char* inputFile);
	
	vector<vector<string> > _process_grammar(char* inputFile);

	bool _in_array(vector<int> find_in, int element);

	int _get_idx_by_symbol(string symbol_to_find);

	vector<int> _push_unique_elements(vector<int> target, vector<int> source);

public:
	// *** public functions ***//
	Grammar();
	void init(char* inputName);
	void parse();
	void print_table();
	

	vector<vector<int> > get_rule(char* inputFile, vector<bool> &func_is_epsilon);

	vector<vector<int> > get_first(vector<bool> local_is_epsilon);

	vector<vector<int> > get_follow(vector<bool> local_is_epsilon, vector<vector<int> > local_first_sets);

	vector<vector<int> > get_predict(vector<bool> local_is_epsilon, vector<vector<int> > local_first_sets, vector<vector<int> > local_follow_sets);
};

using namespace std;


int Grammar::start_symbol = 1;
Grammar::Grammar():maxidx_terminal(0), num_nonterminals(0), num_rules(0) {}

vector<vector<int> > Grammar::get_rule(char* inputFile, vector<bool> &func_is_epsilon) {
	vector<vector<int> > temp_rules(num_rules, vector<int> (maxidx_terminal + num_nonterminals));

	int symbol_pos = 0;	
	int line_pos = 0;  
	string cur_symbol;

	input.open(inputFile);

	// skip terminal define
	while(getline(input, line)) {
		if(line.compare("") == 0) break;
	}

	// process rule
	while(getline(input, line)) {
		symbol_pos = 0;
		
		stringstream ss(line);

		getline(ss, cur_symbol, ' ');

		string left_side = cur_symbol;
		temp_rules[line_pos][symbol_pos] = _get_idx_by_symbol(cur_symbol);
		++symbol_pos;

		getline(ss, cur_symbol, ' ');
		if(ss.eof()) {
	
			func_is_epsilon[_get_idx_by_symbol(left_side) - 1] = true;
		}
		while(getline(ss, cur_symbol, ' ')) {
			temp_rules[line_pos][symbol_pos] = _get_idx_by_symbol(cur_symbol);
			++symbol_pos; 
		}

		++line_pos;
	}
	input.close();

	return temp_rules;
}

vector<vector<int> > Grammar::get_first(vector<bool> local_is_epsilon) {
	// array
	vector<vector<int> > temp_first(num_nonterminals + 1, vector<int>(0));
	bool complete = false;

	while(!complete) {
		complete = true;
		for(int i = 0; i < full_rules.size(); i++) {   // line
			for(int j = 1; j < full_rules[i].size(); j++) {  // symbol of line
				if(full_rules[i][j] == 0) {

					break;

				// full_rules[i][j] - terminal
				} else if(full_rules[i][j] < 0) {

					if(!_in_array(temp_first[full_rules[i][0]], full_rules[i][j])) {
						temp_first[full_rules[i][0]].push_back(abs(full_rules[i][j]));
						complete = false; 
						break;
					} else break;

				} else if(full_rules[i][j] > 0 && temp_first[full_rules[i][j]].size() > 0) {

					vector<int> cur_elements_first = temp_first[full_rules[i][j]];

					for(int k = 0; k < cur_elements_first.size(); k++) {
						if(!_in_array(temp_first[full_rules[i][0]], cur_elements_first[k])) {
							temp_first[full_rules[i][0]].push_back(cur_elements_first[k]);
							complete = false;
						}
					}
				}
				
				if(! local_is_epsilon[abs(full_rules[i][j]) - 1]) {
					break;
				}
			}
		}
	}

	return temp_first;			
}

vector<vector<int> > Grammar::get_follow(vector<bool> local_is_epsilon, vector<vector<int> > local_first_sets ) {
	vector<vector<int> > temp_follow(num_nonterminals + 1, vector<int>(0));
	int count = 0;

	while(count < 20) {

		for(int i = 0; i < full_rules.size(); i++) {
			for(int j = 1; j < full_rules[i].size(); j++) {

				if(full_rules[i][j] < 0) 
					continue;
				// nonterminal
				else if(full_rules[i][j] > 0) {

					for(int k = j + 1; k < full_rules[i].size(); k++) {
						// 
						if(full_rules[i][k] == 0 && temp_follow[full_rules[i][0]].size() != 0) {
							// 
							temp_follow[full_rules[i][j]] = _push_unique_elements(temp_follow[full_rules[i][j]], temp_follow[full_rules[i][0]]);

						} else if(temp_follow[full_rules[i][k]].size() == 0) break;

						// terminal
						else if(full_rules[i][k] < 0) {
							// push
							if(!_in_array(temp_follow[full_rules[i][j]], abs(full_rules[i][k]))) {
								temp_follow[full_rules[i][j]].push_back(abs(full_rules[i][k]));
							}
							break;
						}
						//  full_rules[i][k] - nontermail
						else if(full_rules[i][k] > 0) {
							// 
							temp_follow[full_rules[i][j]] = _push_unique_elements(temp_follow[full_rules[i][j]], local_first_sets[full_rules[i][k]]);

							if(local_is_epsilon[full_rules[i][k] - 1]) continue;
							else break;
						}
					}
				} else break;

			}
		}
		++count;
	}

	return temp_follow;
}

vector<vector<int> > Grammar::get_predict(vector<bool> local_is_epsilon, vector<vector<int> > local_first_sets, vector<vector<int> > local_follow_sets) {
	vector<vector<int> > temp_predict_sets(num_rules, vector<int>(0));
	
	for(int i = 0; i < full_rules.size(); i++) {
		for(int j = 1; j < full_rules[i].size(); j++) {
			if(full_rules[i][j] == 0) {
				// end
				temp_predict_sets[i] = _push_unique_elements(temp_predict_sets[i], local_follow_sets[full_rules[i][0]]); // Only push unique elements into the vector
				break;

			// terminal
			} else if(full_rules[i][j] < 0) { 
				// push
				temp_predict_sets[i].push_back(abs(full_rules[i][j]));
				break;

			// nonterminal
			} else { 
				temp_predict_sets[i] = _push_unique_elements(temp_predict_sets[i], local_first_sets[full_rules[i][j]]); // Only push unique elements
				if(local_is_epsilon[full_rules[i][j] - 1]) {
					if(full_rules[i][j + 1] == 0) {
						temp_predict_sets[i] = _push_unique_elements(temp_predict_sets[i], local_follow_sets[full_rules[i][0]]);
						break;
					}
					continue;
				} else break;
			}
		}
	}

	return temp_predict_sets;
}

vector<int> Grammar::_push_unique_elements(vector<int> target, vector<int> source) {
	for(int i = 0; i < source.size(); i++) {
		if(!_in_array(target, abs(source[i]))) {
			target.push_back(source[i]);
		}
	}
	return target;
}

bool Grammar::_in_array(vector<int> find_in, int element) {

	if(find_in.size() == 0) return false;

	for(int i = 0; i < find_in.size(); i++) {
		if(find_in[i] == abs(element)) return true;
	}
	return false;
}


vector<vector<string> > Grammar::_process_grammar(char* inputFile) {

	vector<string> temp_terminals(maxidx_terminal + 1); // Terminal names
	vector<string> temp_nonterminals(num_nonterminals + 1); // Nonterminal names

	vector< vector<string> > temp_names(2);
	temp_names[0] = temp_terminals;
	temp_names[1] = temp_nonterminals;

	input.open(inputFile); 

	string cur_terminal_name;
	string cur_terminal_idx;

	while(getline(input, line)) {
		if(line.compare("") == 0) break;

		stringstream ss(line); 
		getline(ss, cur_terminal_name, ' '); 
		getline(ss, cur_terminal_idx, ' ');

		// cout<<cur_terminal_idx<<"-"<<cur_terminal_name<<endl;
		temp_names[0][atoi(cur_terminal_idx.c_str())] = cur_terminal_name;
	}

	int cur_nonterminal_idx = 0; 
	string cur_nonterminal_name; 
	string last_unique_nonterminal("");

	while(getline(input, line)) {
		stringstream ss(line); 
		getline(ss, cur_nonterminal_name, ' ');

		if(cur_nonterminal_name.compare(last_unique_nonterminal) != 0) {
			temp_names[1][cur_nonterminal_idx + 1] = cur_nonterminal_name; 
			++cur_nonterminal_idx; 
			last_unique_nonterminal = cur_nonterminal_name;
		}
	}
	input.close();
	return temp_names;
}

void Grammar::_grammar_counter(char* inputFile) {
	input.open(inputFile);

	string cur_maxfreq_terminals;

	while(getline(input, line)){
		if(line.compare("") == 0) break;

		stringstream ss(line);

		getline(ss, cur_maxfreq_terminals, ' ');
		getline(ss, cur_maxfreq_terminals, ' ');

		maxidx_terminal = atoi(cur_maxfreq_terminals.c_str());
	}

	string last_unique_nonterminal(""); 
	string this_nonterminal;

	while(getline(input, line)) {
		stringstream ss(line); 
		getline(ss, this_nonterminal, ' ');

		if(this_nonterminal.compare(last_unique_nonterminal) != 0) {
			++num_nonterminals;
			last_unique_nonterminal = this_nonterminal;
		}

		++num_rules;
	}

	input.close();
}

int Grammar::_get_idx_by_symbol(string symbol_to_find) {

	for(int i = 0; i < terminal_names.size(); i++) {
		if(terminal_names[i].compare(symbol_to_find) == 0)
			return i * -1;
	}

	for(int i = 0; i < nonterminal_names.size(); i++) {
		if(nonterminal_names[i].compare(symbol_to_find) == 0)
			return i;
	}

	return 999;
}

void Grammar::init(char* inputName) {
	fflush(stdout);
	_grammar_counter(inputName); 

	vector<vector<string> > nonterm_and_term_names = _process_grammar(inputName); 
	terminal_names = nonterm_and_term_names[0]; 
	nonterminal_names = nonterm_and_term_names[1];

	vector<bool> is_epsilon(num_nonterminals); 
	full_rules = get_rule(inputName, is_epsilon); 
	first_sets = get_first(is_epsilon);
	follow_sets = get_follow(is_epsilon, first_sets);
	predict_sets = get_predict(is_epsilon, first_sets, follow_sets);

	parse_table.resize(nonterminal_names.size(), vector<int>(maxidx_terminal));
	for(int i = 0; i < nonterminal_names.size(); i++) {
		for(int j = 0; j < maxidx_terminal; j++) {
			parse_table[i][j] = 0;
		}
	}

	for(int i = 0; i < predict_sets.size(); i++) {
		for(int j = 0; j < predict_sets[i].size(); j++) {
			if(parse_table[full_rules[i][0] - 1][predict_sets[i][j] - 1]) {
				cout << "Error: table exist!";
				exit(0);
			}
			parse_table[full_rules[i][0] - 1][predict_sets[i][j] - 1] = i + 1;
		}
	}

	right_hand_sides.resize(full_rules.size(), vector<int>(0));
	for(int i = 0; i < full_rules.size(); i++) {
		for(int j = full_rules[i].size() - 1; j > 0; j--) {
			if(full_rules[i][j] != 0) right_hand_sides[i].push_back(full_rules[i][j]); 
		}
		right_hand_sides[i].push_back(0); 
	}
}

void Grammar::parse() {
    stack<int> parse_stack;
    int expected_symbol;
    token input_tok;
    parse_stack.push(start_symbol);
    input_tok = scan();

    do {
    	expected_symbol = parse_stack.top();
    	if(input_tok.num == 32) {
    		cout << "\nError: Invalid token \"" << input_tok.image << "\" at row: " << input_tok.line << " column: " << input_tok.column <<endl;
    		exit(0);
    		input_tok = scan();
    	}
    	if(expected_symbol < 0) {
    		if(input_tok.num == abs(expected_symbol)) {    			
    			cout << "Match: " << input_tok.image << "\n";
    			input_tok = scan(); 
    			parse_stack.pop();
    		} else {
    			cout <<"Error: \"" << input_tok.image << "\" at row: " << input_tok.line << " column: " << input_tok.column << " was not matched with expected \"" << terminal_names[abs(expected_symbol)] <<"\""<<endl;
    			exit(0);
    			parse_stack.pop();
    		}
    	} else if (expected_symbol > 0) {

    		if(parse_table[abs(expected_symbol)-1][input_tok.num-1] == 0) {
    			vector<int> this_nonterminals_first = first_sets[expected_symbol];
    			vector<int> this_nonterminals_follow = follow_sets[expected_symbol];
    			cout << "Error: Invalid input for symbol pair: (" << nonterminal_names[expected_symbol] << ", " << input_tok.image << ")"<<endl;
    			exit(0);
    			do {
    				input_tok = scan();
    				if(_in_array(this_nonterminals_first, input_tok.num)) {
    					cout << "\tInput token: \"" << input_tok.image << "\" was found in \"" << nonterminal_names[expected_symbol] << "\"\'s first set."<<endl;
    					break;
    				} else if (_in_array(this_nonterminals_follow, input_tok.num)) {
    					cout << "\tInput token: \"" << input_tok.image << "\" was found in \"" << nonterminal_names[expected_symbol] << "\"'s first set."<<endl;
    					parse_stack.pop();
    					break;
    				}
    			} while(input_tok.num != tok_eof);
    		} else {
    			vector<int> pred_rule = right_hand_sides[parse_table[abs(expected_symbol) - 1][input_tok.num - 1] - 1];


    			cout << "Predicted: " << nonterminal_names[expected_symbol] << " -> ";
				parse_stack.pop();

				int i = 0;
				while(pred_rule[i] != 0) {
					parse_stack.push(pred_rule[i]);
					++i;
				}

				for(i = i-1; i >= 0; i--) {
					if(pred_rule[i] < 0)
						cout << terminal_names[abs(pred_rule[i])] << " ";
					else
						cout << nonterminal_names[abs(pred_rule[i])] << " ";
				}
				i = 0; 
				cout << endl; 

			}
    	}
    } while (parse_stack.top() != -1);
    cout << "succeed!" << endl;
}

void Grammar::print_table(){
    cout<<"+++++++++++++++++ parse table ++++++++++++++++++++++"<<endl<<endl;

	int n_idx, t_idx, rule_idx = 0;
	for(int i = 0; i < predict_sets.size(); i++) {
		n_idx = full_rules[i][0];
		rule_idx = i;

		if ( nonterminal_names[n_idx] == " "){
			continue;
		}
		// nonterminal
		cout<< nonterminal_names[n_idx]<<"\t\t";

		for(int j = 0; j < predict_sets[i].size(); j++) {

			t_idx = predict_sets[i][j];

			cout<< terminal_names[t_idx] << " | ";

			cout << nonterminal_names[n_idx] << " -> ";


			vector<int> pred_rule = right_hand_sides[rule_idx];

			int k = 0;
			while(pred_rule[k] != 0 && pred_rule[k] != 999) {
				++k;
			}

			for(k = k-1; k >= 0; k--) {
				if(pred_rule[k] < 0)
					cout << terminal_names[abs(pred_rule[k])] << " ";
				else
					cout << nonterminal_names[abs(pred_rule[k])] << " ";
			}
			cout<< " # ";
			k = 0; 
		}
		cout<<endl<<endl;
	}
}

int main(int argc, char* argv[]) {
	Grammar g;
	g.init(argv[1]);
	g.parse();
	//g.print_table();
	return 0;
}
