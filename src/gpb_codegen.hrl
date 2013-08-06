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

-endif. %% gpb_codegen_hrl.
