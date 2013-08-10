-ifndef(gpb_codegen_hrl).
-define(gpb_codegen_hrl, true).

-compile({parse_transform,gpb_codegen}).

-define(case_clause(Clause),
        gpb_codegen:case_clause(case dummy of Clause end)).

-define(case_clause(Clause, Transforms),
        gpb_codegen:case_clause(case dummy of Clause end,
                                Transforms)).

-define(expr(X),
        gpb_codegen:expr(X)).

-define(expr(X, Transforms),
        gpb_codegen:expr(X, Transforms)).

-define(exprs(X1, Transforms),
        gpb_codegen:exprs(X1, Transforms)).

-define(exprs(X1, X2, Transforms),
        gpb_codegen:exprs(X1, X2, Transforms)).

-define(exprs(X1, X2, X3, Transforms),
        gpb_codegen:exprs(X1, X2, X3, Transforms)).

-define(exprs(X1, X2, X3, X4, Transforms),
        gpb_codegen:exprs(X1, X2, X3, X4, Transforms)).

-define(exprs(X1, X2, X3, X4, X5, Transforms),
        gpb_codegen:exprs(X1, X2, X3, X4, X5, Transforms)).

-endif. %% gpb_codegen_hrl.
