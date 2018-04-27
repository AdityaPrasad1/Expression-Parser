%DISCLAIMER: This code is horrible looking and we're sorry Loc/Professor 
%Rellermeyer/anyone who reads this.

%Calvin Liu and Aditya Prasad

% CS 345 Assignment 2 Starter Code

% This code fully implements the expression and term parsing for
% mySIMPL. No modifications to the parse code itself should be necessary.
% It will also return to you an AST node for the expression. The only
% thing that might make this an issue is that you may not want to use
% the AST format provided. You are free to change it to your own 
% implementation.
%
% If there are questions about this code, please feel free to ask
% the Dr. Rellermeyer or the TA.

% parse: note you will have to change this to have it work for general
% programs; at the moment, it will only parse expressions and return to
% you an expression node in the AST variable
parse(TokenList, AST) :- phrase(prog(AST), TokenList, []).

evaluate(AST, Result) :- empty_assoc(List), evaluateProg(AST, List, Result).

% base
% : NOTE THIS IS INCOMPLETE; still need to have with ID and expression 
% parsing
% This says to parse anything in the token list as long as that thing is 
% a number (i.e. bind N to the token, but then check if it is a number 
base(base(N)) --> [N], {number(N)}.
base(base(Expression)) --> ['('], expression(Expression), [')'].
base(base(Id)) --> idrule(Id). 
% Id Rule
idrule(id(Id)) --> [Id], {\+ (Id == ['return']), \+ (Id == ['var']), \+ (number(Id)), \+ (Id == ['/']),\+ (Id == ['-']),\+ (Id == ['+']), \+ (Id == ['*']), \+ (Id == ['(']), \+ (Id == [')'])}.

% prog
% Need cases for retStatement(1), declaration(2), assignment(2), declassignemnt(2)
prog(prog(Ret)) --> retStatement(Ret), ['.'].
prog(prog(Dec,Prog)) --> declaration(Dec), [';'], prog(Prog).
prog(prog(Assi,Prog)) --> assignment(Assi), [';'], prog(Prog).
prog(prog(Decl,Prog)) --> declAssignment(Decl), [';'], prog(Prog).

% declaration
declaration(declaration(N)) --> ['var'], idrule(N).
 
% assignment
assignment(assignment(ID, Base) ) --> idrule(ID), ['<-'], base(Base).

% declAssignment
declAssignment(declAssignment(N,Base)) --> ['var'], idrule(N), ['<-'], base(Base).

% retStatement
retStatement(retStatement(N)) --> ['return'], base(N).

% simple case: just a term
expression(expression(T)) --> term(T).
% recursive case: expression then term. Since we can't do the following...
% expression --> expression ...
% due to left recursion, we have to build up the expression by parsing
% a term and passing this term along to the left_assoc call which
% will build the expression node for us
expression(E) --> term(T1), addOp(AO), left_assoc(E, expression(T1), AO).

% left assoc base case: there is only a term left, so we take what was
% already passed in with the newly parsed term and form the expression
% node
left_assoc(expression(T1, AO, T2), T1, AO) --> term(T2).
% left assoc recursive case; there is more than just a term left, so
% we must continue parsing using left_assoc
left_assoc(E, T1, AO) --> term(T2), addOp(AO2), 
                          left_assoc(E, expression(T1, AO, T2), AO2).

% simple case: just a factor
term(term(F)) --> factor(F).
% recursive case: like the expression case, we can't have left-recursion
% so we build up the parse factor by factor
term(T) --> factor(F1), mulOp(MO), left_assoc_term(T, term(F1), MO).

% left assoc term base case
left_assoc_term(term(F1, MO, F2), F1, MO) --> factor(F2).
% left assoc term recursive case
left_assoc_term(T, F1, MO) --> factor(F2), mulOp(MO2), 
                               left_assoc_term(T, term(F1, MO, F2), MO2).

% factor parse; a factor is merely a base
factor(factor(B)) --> base(B).

% parse add ops and mul ops; parses the operation, and unifies the 
% passed in variable to an op node
addOp(addOp('+')) --> ['+'].
addOp(addOp('-')) --> ['-'].
mulOp(mulOp('*')) --> ['*'].
mulOp(mulOp('/')) --> ['/'].


% You need to finish adding the rest of the parsing rules. 
% After that, you need to define evaluation rules that will take an AST and
% "run" your program.

% evaluate(AST, Result) :- empty_assoc(List), evaluateProg(AST, List, Result).

evaluateProg(AST, List, Result) :-
	prog(Tree) = AST,
	evalRet(Tree, List, Result).

evaluateProg(AST, List, Result) :-
	prog(Tree,Rest) = AST,
	evalDec(Tree, List, Outlist),
	evaluateProg(Rest, Outlist, Result).

evaluateProg(AST, List, Result) :-
	prog(Tree,Rest) = AST,
	evalAssignment(Tree, List, Outlist),
	evaluateProg(Rest, Outlist, Result).

evaluateProg(AST, List, Result) :-
	prog(Tree,Rest) = AST,
	evalDecl(Tree, List, Outlist),
	evaluateProg(Rest, Outlist, Result).

evalRet(AST, List, Result) :- % Return a number into Result
	retStatement(Tree) = AST,
	evalBase(Tree, List, Result).

% Need three cases for base
% One base will return just a simple case
% One base will return a id case
% Last base will deal with expression

evalBase(AST,List, Result) :- % Handle id case
	base(Tree) = AST,
	id(Next) = Tree,
    get_assoc(Next, List, Result).

evalBase(AST,_, Result) :- % Handle num case
	base(Tree) = AST,
	number(Tree),
	Result = Tree.

evalBase(AST,List, Result) :- % Handle expression case
	base(Tree) = AST,
	evalExpression(Tree, List, Result).

%evalExpression and evalTerm code
evalExpression(Tree, List, Result):- % one argument with addOp
	expression(Next) = Tree,
	term(factor(Ter)) = Next,
	evalBase(Ter, List, Result).

evalExpression(Tree, List, Result) :- % arguments with addOp
	expression(Next, Op, Rest) = Tree,
	evalExpression(Next, List, V1),
	evalTerm(Rest, List, V2),
	ope(Op,V1,V2,Res),
	Result is Res.

evalTerm(Tree, List, Result) :-	% addOp
	term(factor(Ter2)) = Tree,
	evalBase(Ter2,List, Result).

%mulOp code
evalExpression(Tree, List, Result):- % three arguments with mulOp
	expression(Tree2) = Tree,
	term(Next, Op, Rest) = Tree2,
	evalExpression(Next, List, V1),
	evalTerm(Rest, List, V2),
	ope(Op,V1,V2,Res),
	Result is Res.

evalExpression(Tree, List, Result):- % one argument with mulOp
	term(factor(Next)) = Tree,
	evalBase(Next, List, Result).


evalTerm(Tree, List, Result) :-	% mulOp
	factor(Ter2) = Tree,
	evalBase(Ter2,List, Result).

%base case
evalExpression(Tree, List, Result):-
	evalBase(Tree, List, Result).

%operator code
ope(Op, V1, V2, Res) :-
	addOp(RealOp) = Op,
	RealOp = '+',
	Res is V1 + V2.

ope(Op, V1, V2, Res) :-
	addOp(RealOp) = Op,
	RealOp = '-',
	Res is V1 - V2.

ope(Op, V1, V2, Res) :-
	mulOp(RealOp) = Op,
	RealOp = '*',
	Res is V1 * V2.

ope(Op, V1, V2, Res) :-
	mulOp(RealOp) = Op,
	RealOp = '/',
	Res is div(V1,V2).

%declaration
evalDec(AST, List, Outlist) :-
	declaration(id(Tree)) = AST,
	 % Put variable in assoc list

	\+ get_assoc(Tree, List,_),
	put_assoc(Tree, List, 'dead', Outlist).

%assignment
evalAssignment(AST, List, Outlist) :- 
	assignment(Tree, Rest) = AST,
	% Assign value to variable and store it in out list

	id(Next) = Tree,
	evalExpression(Rest,List,Val),
	put_assoc(Next,List,Val,Outlist).

%declAssignment
evalDecl(AST, List, Outlist) :-
	declAssignment(Tree, Rest) = AST,
	% Put variable in assoc list and assign value to variable and store it in out list

	id(Next) = Tree,
	evalExpression(Rest,List,Val),
	put_assoc(Next,List,Val,Outlist).
