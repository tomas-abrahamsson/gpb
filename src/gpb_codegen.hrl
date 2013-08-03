-ifndef(gpb_codegen_hrl).
-define(gpb_codegen_hrl, true).

-compile({parse_transform,gpb_codegen}).

-define(case_clause(Clause),
        gpb_codegen:case_clause(case dummy of Clause end)).

-endif. %% gpb_codegen_hrl.
