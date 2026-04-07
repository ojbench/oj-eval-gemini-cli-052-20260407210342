#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cmath>

using namespace std;

struct Term {
    long long a, b, c, d; // a * x^b * sin^c x * cos^d x
    bool operator<(const Term& o) const {
        if (b != o.b) return b > o.b;
        if (c != o.c) return c > o.c;
        return d > o.d;
    }
    bool operator==(const Term& o) const {
        return b == o.b && c == o.c && d == o.d;
    }
};

struct Poly {
    vector<Term> terms;
    
    void simplify() {
        if (terms.empty()) return;
        std::sort(terms.begin(), terms.end());
        vector<Term> res;
        for (auto t : terms) {
            if (res.empty()) {
                res.push_back(t);
            } else {
                if (res.back() == t) {
                    res.back().a += t.a;
                } else {
                    res.push_back(t);
                }
            }
        }
        terms.clear();
        for (auto t : res) {
            if (t.a != 0) terms.push_back(t);
        }
    }
};

Poly add(const Poly& p1, const Poly& p2) {
    Poly res;
    res.terms = p1.terms;
    res.terms.insert(res.terms.end(), p2.terms.begin(), p2.terms.end());
    res.simplify();
    return res;
}

Poly sub(const Poly& p1, const Poly& p2) {
    Poly res;
    res.terms = p1.terms;
    for (auto t : p2.terms) {
        t.a = -t.a;
        res.terms.push_back(t);
    }
    res.simplify();
    return res;
}

Poly mul(const Poly& p1, const Poly& p2) {
    Poly res;
    for (auto t1 : p1.terms) {
        for (auto t2 : p2.terms) {
            res.terms.push_back({t1.a * t2.a, t1.b + t2.b, t1.c + t2.c, t1.d + t2.d});
        }
    }
    res.simplify();
    return res;
}

Poly derivate(const Poly& p) {
    Poly res;
    for (auto t : p.terms) {
        if (t.b > 0) {
            res.terms.push_back({t.a * t.b, t.b - 1, t.c, t.d});
        }
        if (t.c > 0) {
            res.terms.push_back({t.a * t.c, t.b, t.c - 1, t.d + 1});
        }
        if (t.d > 0) {
            res.terms.push_back({-t.a * t.d, t.b, t.c + 1, t.d - 1});
        }
    }
    res.simplify();
    return res;
}

struct Frac {
    Poly num, den;
};

Frac add(const Frac& f1, const Frac& f2) {
    return {add(mul(f1.num, f2.den), mul(f2.num, f1.den)), mul(f1.den, f2.den)};
}

Frac sub(const Frac& f1, const Frac& f2) {
    return {sub(mul(f1.num, f2.den), mul(f2.num, f1.den)), mul(f1.den, f2.den)};
}

Frac mul(const Frac& f1, const Frac& f2) {
    return {mul(f1.num, f2.num), mul(f1.den, f2.den)};
}

Frac div(const Frac& f1, const Frac& f2) {
    return {mul(f1.num, f2.den), mul(f1.den, f2.num)};
}

Frac derivate(const Frac& f) {
    Poly num_prime = derivate(f.num);
    Poly den_prime = derivate(f.den);
    Poly new_num = sub(mul(num_prime, f.den), mul(den_prime, f.num));
    Poly new_den = mul(f.den, f.den);
    return {new_num, new_den};
}

string format_term(const Term& t, bool is_first) {
    string res = "";
    if (t.a < 0) res += "-";
    else if (!is_first) res += "+";
    
    long long abs_a = std::abs(t.a);
    bool is_const = (t.b == 0 && t.c == 0 && t.d == 0);
    
    if (abs_a != 1 || is_const) {
        res += std::to_string(abs_a);
    }
    
    if (t.b > 0) {
        res += "x";
        if (t.b > 1) res += "^" + std::to_string(t.b);
    }
    if (t.c > 0) {
        res += "sin";
        if (t.c > 1) res += "^" + std::to_string(t.c);
        res += "x";
    }
    if (t.d > 0) {
        res += "cos";
        if (t.d > 1) res += "^" + std::to_string(t.d);
        res += "x";
    }
    return res;
}

string to_string(const Poly& p) {
    if (p.terms.empty()) return "0";
    string res = "";
    for (size_t i = 0; i < p.terms.size(); ++i) {
        res += format_term(p.terms[i], i == 0);
    }
    return res;
}

bool is_one(const Poly& p) {
    return p.terms.size() == 1 && p.terms[0].a == 1 && p.terms[0].b == 0 && p.terms[0].c == 0 && p.terms[0].d == 0;
}

void print_frac(const Frac& f) {
    if (f.num.terms.empty()) {
        cout << "0\n";
        return;
    }
    if (is_one(f.den)) {
        cout << to_string(f.num) << "\n";
        return;
    }
    
    string s1 = to_string(f.num);
    string s2 = to_string(f.den);
    
    if (f.num.terms.size() > 1) s1 = "(" + s1 + ")";
    if (f.den.terms.size() > 1) s2 = "(" + s2 + ")";
    
    cout << s1 << "/" << s2 << "\n";
}

enum TokenType {
    T_FRAC, T_ADD, T_SUB, T_MUL, T_DIV, T_LPAREN, T_RPAREN
};

struct Token {
    TokenType type;
    Frac val;
};

vector<Token> tokenize(const string& s) {
    vector<Token> tokens;
    int i = 0;
    int n = s.length();
    while (i < n) {
        if (s[i] == ' ') { i++; continue; }
        if (s[i] == '+') { tokens.push_back({T_ADD, {}}); i++; }
        else if (s[i] == '-') { tokens.push_back({T_SUB, {}}); i++; }
        else if (s[i] == '*') { tokens.push_back({T_MUL, {}}); i++; }
        else if (s[i] == '/') { tokens.push_back({T_DIV, {}}); i++; }
        else if (s[i] == '(') { tokens.push_back({T_LPAREN, {}}); i++; }
        else if (s[i] == ')') { tokens.push_back({T_RPAREN, {}}); i++; }
        else if (isdigit(s[i])) {
            long long val = 0;
            while (i < n && isdigit(s[i])) {
                val = val * 10 + (s[i] - '0');
                i++;
            }
            Frac f;
            f.num.terms.push_back({val, 0, 0, 0});
            f.den.terms.push_back({1, 0, 0, 0});
            tokens.push_back({T_FRAC, f});
        }
        else if (s[i] == 'x') {
            long long pow = 1;
            i++;
            if (i < n && s[i] == '^') {
                i++;
                pow = 0;
                while (i < n && isdigit(s[i])) {
                    pow = pow * 10 + (s[i] - '0');
                    i++;
                }
            }
            Frac f;
            f.num.terms.push_back({1, pow, 0, 0});
            f.den.terms.push_back({1, 0, 0, 0});
            tokens.push_back({T_FRAC, f});
        }
        else if (i + 2 < n && s.substr(i, 3) == "sin") {
            i += 3;
            long long pow = 1;
            if (i < n && s[i] == '^') {
                i++;
                pow = 0;
                while (i < n && isdigit(s[i])) {
                    pow = pow * 10 + (s[i] - '0');
                    i++;
                }
            }
            if (i < n && s[i] == 'x') i++; // consume 'x'
            Frac f;
            f.num.terms.push_back({1, 0, pow, 0});
            f.den.terms.push_back({1, 0, 0, 0});
            tokens.push_back({T_FRAC, f});
        }
        else if (i + 2 < n && s.substr(i, 3) == "cos") {
            i += 3;
            long long pow = 1;
            if (i < n && s[i] == '^') {
                i++;
                pow = 0;
                while (i < n && isdigit(s[i])) {
                    pow = pow * 10 + (s[i] - '0');
                    i++;
                }
            }
            if (i < n && s[i] == 'x') i++; // consume 'x'
            Frac f;
            f.num.terms.push_back({1, 0, 0, pow});
            f.den.terms.push_back({1, 0, 0, 0});
            tokens.push_back({T_FRAC, f});
        }
        else {
            i++; // should not reach here for valid input
        }
    }
    return tokens;
}

vector<Token> insert_implicit_mul(const vector<Token>& tokens) {
    vector<Token> res;
    for (size_t i = 0; i < tokens.size(); ++i) {
        if (i > 0) {
            TokenType prev = tokens[i-1].type;
            TokenType curr = tokens[i].type;
            bool prev_val = (prev == T_FRAC || prev == T_RPAREN);
            bool curr_val = (curr == T_FRAC || curr == T_LPAREN);
            if (prev_val && curr_val) {
                res.push_back({T_MUL, {}});
            }
        }
        res.push_back(tokens[i]);
    }
    return res;
}

vector<Token> handle_unary(const vector<Token>& tokens) {
    vector<Token> res;
    for (size_t i = 0; i < tokens.size(); ++i) {
        if (tokens[i].type == T_SUB || tokens[i].type == T_ADD) {
            if (i == 0 || tokens[i-1].type == T_LPAREN) {
                Frac zero;
                zero.den.terms.push_back({1, 0, 0, 0});
                res.push_back({T_FRAC, zero});
            }
        }
        res.push_back(tokens[i]);
    }
    return res;
}

Frac apply_op(Frac a, Frac b, TokenType op) {
    if (op == T_ADD) return add(a, b);
    if (op == T_SUB) return sub(a, b);
    if (op == T_MUL) return mul(a, b);
    if (op == T_DIV) return div(a, b);
    return a;
}

int precedence(TokenType op) {
    if (op == T_ADD || op == T_SUB) return 1;
    if (op == T_MUL || op == T_DIV) return 2;
    return 0;
}

Frac parse_expr(const vector<Token>& tokens) {
    vector<Frac> values;
    vector<TokenType> ops;
    
    for (auto token : tokens) {
        if (token.type == T_FRAC) {
            values.push_back(token.val);
        } else if (token.type == T_LPAREN) {
            ops.push_back(token.type);
        } else if (token.type == T_RPAREN) {
            while (!ops.empty() && ops.back() != T_LPAREN) {
                Frac val2 = values.back(); values.pop_back();
                Frac val1 = values.back(); values.pop_back();
                TokenType op = ops.back(); ops.pop_back();
                values.push_back(apply_op(val1, val2, op));
            }
            if (!ops.empty()) ops.pop_back(); // pop '('
        } else {
            while (!ops.empty() && precedence(ops.back()) >= precedence(token.type)) {
                Frac val2 = values.back(); values.pop_back();
                Frac val1 = values.back(); values.pop_back();
                TokenType op = ops.back(); ops.pop_back();
                values.push_back(apply_op(val1, val2, op));
            }
            ops.push_back(token.type);
        }
    }
    
    while (!ops.empty()) {
        Frac val2 = values.back(); values.pop_back();
        Frac val1 = values.back(); values.pop_back();
        TokenType op = ops.back(); ops.pop_back();
        values.push_back(apply_op(val1, val2, op));
    }
    
    return values.back();
}

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);
    string s;
    if (cin >> s) {
        vector<Token> tokens = tokenize(s);
        tokens = insert_implicit_mul(tokens);
        tokens = handle_unary(tokens);
        Frac f = parse_expr(tokens);
        print_frac(f);
        Frac df = derivate(f);
        print_frac(df);
    }
    return 0;
}
