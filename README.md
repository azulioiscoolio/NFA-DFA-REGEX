# NFA-DFA-REGEX

The type nfa_t is the type representing NFAs. It is modeled after the formal definition of an NFA, a 5-tuple (Q, Σ, δ, q0, F) where:

Q is a finite set of states,
Σ is a finite alphabet,
δ : Q × (Σ ∪ {ε}) → P(Q) is the transition function,
q0 ∈ Q is the start state, and
F ⊆ Q is the set of accept states.
We translate this definition into OCaml in a straightforward way using record syntax:

type ('q, 's) transition = 'q * 's option * 'q
type ('q, 's) nfa = {
    qs : 'q list;
    sigma : 's list;
    delta : ('q, 's) transition list;
    q0 : 'q;
    fs : 'q list;
}
Notice the types are parametric in state 'q and symbol 's.

The type transition represents NFA transitions. For example:

let t1 = (0, Some c, 1) (* Transition from state 0 to state 1 on character 'c' *)
let t2 = (1, None, 0)   (* Transition from state 1 to state 0 on epsilon *)
An example NFA would be:

let m = {
    qs = [0;1;2];
    sigma = ['a'];
    delta = [(0, Some 'a', 1); (1, None, 2)];
    q0 = 0;
    fs = [2]
}
This looks like:

NFA m

Here is a DFA:

let n = {
    qs = [0;1;2];
    sigma = ['a';'b';'c'];
    delta = [(0, Some 'a', 1); (1, Some 'b', 0); (1, Some 'c', 2)];
    q0 = 0;
    fs = [2]
}
This looks like:

NFA n

Utility Functions
We have included correct implementations of the set functions from P2A. You may use these in your solution. We have also provided a number of other potentially useful functions.

explode s

Type: string -> char list
Description: Takes a string as input and returns representation of the string as a list of characters.
fix comp f x0

Type: ('a -> 'a -> bool) -> ('a -> 'a) -> 'a -> 'a
Description: Takes an equality predicate comp, function f, and initial value x0 and computes the fixpoint of f by iteration starting from x0 and using comp to determine equality. (This might be helpful if you decide to implement e_closure and nfa_to_dfa by fixpoint iteration. The following document may be a useful reference.)
Part 1: NFAs
You must implement the following functions as specified.

move m l c

Type: ('q, 's) nfa_t -> 'q list -> 's option -> 'q list
Description: This function takes as input an NFA, a list of initial states, and a symbol option. The output will be a list of states (in any order, with no duplicates) that the NFA might be in after making one transition on the symbol (or epsilon if None), starting from one of the initial states given as an argument to move.
Examples:
move m [0] (Some 'a') = [1] (* m is the NFA defined above *)
move m [1] (Some 'a') = []
move m [2] (Some 'a') = []
move m [0;1] (Some 'a')  = [1]
move m [1] None = [2]
Explanation:
Move on m from 0 with Some a returns [1] since from 0 to 1 there is a transition with character a.
Move on m from 1 with Some a returns [] since from 1 there is no transition with character a.
Move on m from 2 with Some a returns [] since from 2 there is no transition with character a.
Move on m from 0 and 1 with Some a returns [1] since from 0 to 1 there is a transition with character a but from 1 there was no transition with character a.
Notice that the NFA uses an implicit dead state. If s is a state in the input list and there are no transitions from s on the input character, then all that happens is that no states are added to the output list for s.
Move on m from 1 with None returns [2] since from 1 to 2 there is an epsilon transition.
e_closure m l

Type: ('q, 's) nfa_t -> 'q list -> 'q list
Description: This function takes as input an NFA and a list of states. The output will be a list of states (in any order, with no duplicates) that the NFA might be in making zero or more epsilon transitions, starting from the list of initial states given as an argument to e_closure.
Examples:
e_closure m [0] = [0] (* where m is the NFA created above *)
e_closure m [1] = [1;2]
e_closure m [2]  = [2]
e_closure m [0;1] = [0;1;2]
Explanation:
e_closure on m from 0 returns [0] since there is no where to go from 0 on an epsilon transition.
e_closure on m from 1 returns [1;2] since from 1 you can get to 2 on an epsilon transition.
e_closure on m from 2 returns [2] since there is no where to go from 2 on an epsilon transition.
accept m s

Type: ('q, char) nfa_t -> string -> bool
Description: This function takes an NFA and a string, and returns true if the NFA accepts the string, and false otherwise. You will find the functions in the String module to be helpful. (Hint: You should implement this function without using nfa_to_dfa.)
Examples:
accept n "" = false  (* n is the NFA defined above *)
accept n "ac" = true
accept n "abc" = false
accept n "abac" = true
Explanation:
accept on n with the string "" returns false because initially we are at our start state 0 and there are no characters to exhaust and we are not in a final state.
accept on n with the string "ac" returns true because from 0 to 1 there is an 'a' transition and from 1 to 2 there is a 'c' transition and now that the string is empty and we are in a final state thus the nfa accepts "ac".
accept on n with the string "abc" returns false because from 0 to 1 there is an 'a' transition but then to use the 'b' we go back from 1 to 0 and we are stuck because we need a 'c' transition yet there is only an 'a' transition. Since we are not in a final state thus the function returns false.
accept on n with the string "abac" returns true because from 0 to 1 there is an 'a' transition but then to use the 'b' we go back from 1 to 0 and then we take an 'a' transition to go to state 1 again and then finally from 1 to 2 we exhaust our last character 'c' to make it to our final state. Since we are in a final state thus the nfa accepts "abac". 4. eclosure on m from 0 and 1 returns [0;1;2] since from 0 you can only get to yourself and from 1 you can get to 2 on an epsilon transition but from 2 you can't go anywhere.
nfa_to_dfa m

Type: ('q, 's) nfa_t -> ('q list, 's) nfa_t
Description: This function takes as input an NFA and converts it to an equivalent DFA. Notice the return type is an nfa_t. This is not a typo, every DFA is an NFA (not the other way around though), a restricted kind of NFA. Namely, it may not have non-deterministic transitions (i.e. epsilon transitions or more than one transition out of a state with the same symbol). The language recognized by an NFA is invariant under nfa_to_dfa. In other words, for all NFAs m and for all strings s, accept m s = accept (nfa_to_dfa m) s.
Part 2: Regular Expressions
The Regexp module contains the following type declaration:

type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp * regexp
  | Concat of regexp * regexp
  | Star of regexp
Here regexp_t is a user-defined OCaml variant datatype representing regular expressions

Empty_String represents the regular expression recognizing the empty string (not the empty set!). Written as a formal regular expression, this would be epsilon.
Char c represents the regular expression that accepts the single character c. Written as a formal regular expression, this would be c.
Union (r1, r2) represents the regular expression that is the union of r1 and r2. For example, Union(Char 'a', Char'b') is the same as the formal regular expression a|b.
Concat (r1, r2) represents the concatenation of r1 followed by r2. For example, Concat(Char 'a', Char 'b') is the same as the formal regular expresion ab.
Star r represents the Kleene closure of regular expression r. For example, Star (Union (Char 'a', Char 'b')) is the same as the formal regular expression (a|b)*.
You must implement your own function to convert a regular expression (in the above format) to an NFA, which you can then use to match particular strings (by leveraging your Nfa module). You must also implement a function that turns regexp_t structures back into a string representation.

regexp_to_nfa re

Type: regexp_t -> nfa_t
Description: This function takes a regexp and returns an NFA that accepts the same language as the regular expression. Notice that as long as your NFA accepts the correct language, the structure of the NFA does not matter since the NFA produced will only be tested to see which strings it accepts.
regexp_to_string re

Type: regexp_t -> string
Description: This function takes a regular expression and returns a string representation of the regular expression in standard infix notation. How to deal with associativity and precedence is up to you - your output will be tested by running it back through the parser to check that your generated string is equivalent to the original regular expression, so excess parentheses will not be penalized.
Examples:
regexp_to_string (Char 'a') = "a"
regexp_to_string (Union (Char 'a', Char 'b')) = "a|b"
regexp_to_string (Concat(Char 'a',Char 'b')) = "ab"
regexp_to_string (Concat(Char 'a',Concat(Char 'a',Char 'b'))) = "aab"
regexp_to_string (Star(Union(Char 'a',Empty_String))) = "(a|E)*" (* Note that 'E' represents epsilon! *)
regexp_to_string (Concat(Star(Union(Char 'a',Empty_String)),Union(Char 'a',Char 'b'))) = "(a|E)*(a|b)"
Hint: You can do this as an in-order DFS traversal over the regexp data structure.
The rest of these functions are implemented for you as helpers. However, they rely on your code for correctness!

string_to_nfa s

Type: string -> nfa
Description: This function takes a string for a regular expression, parses the string, converts it into a regexp, and transforms it to an nfa, using your regexp_to_nfa function. As such, for this function to work, your regexp_to_nfa function must be working. In the starter files we have provided function string_to_regexp that parses strings into regexp values, described next.
string_to_regexp s (provided for you)

Type: string -> regexp
Description: This function takes a string for a regular expression, parses the string, and outputs its equivalent regexp. If the parser determines that the regular expression has illegal syntax, it will raise an IllegalExpression exception.
Examples:
string_to_regexp "a" = Char 'a'
string_to_regexp "(a|b)" = Union (Char 'a', Char 'b')
string_to_regexp "ab" = Concat(Char 'a',Char 'b')
string_to_regexp "aab" = Concat(Char 'a',Concat(Char 'a',Char 'b'))
string_to_regexp "(a|E)*" = Star(Union(Char 'a',Empty_String))
string_to_regexp "(a|E)*(a|b)" = Concat(Star(Union(Char 'a',Empty_String)),Union(Char 'a',Char 'b'))

In a call to string_to_regexp s the string s may contain only parentheses, |, *, a-z (lowercase), and E (for epsilon). A grammatically ill-formed string will result in IllegalExpression being thrown. Note that the precedence for regular expression operators is as follows, from highest(1) to lowest(4):

Precedence	Operator	Description
1	()	parentheses
2	*	closure
3		concatenation
4	|	union
Also, note that all the binary operators are right associative.
