-ifndef(GPB_NIF_TEST_HELPERS__HRL).
-define(GPB_NIF_TEST_HELPERS__HRL, true).

-define(nif_if_supported(FnName),
        fun() ->
                ExtraChecks = try FnName(extra_checks)
                              catch error:function_clause -> []
                              end,
                Needed = FnName(features),
                case gpb_compile_tests:check_nif_features_supported(
                       Needed, ExtraChecks) of
                    ok ->
                        {FnName(title), fun FnName/0};
                    {error, Why} ->
                        Skipping = "Skipping \"" ++ FnName(title) ++ "\"",
                        Title = Skipping ++ ": " ++ Why,
                        {Title, []}
                end
        end()).

-endif. %% -ifndef(GPB_NIF_TEST_HELPERS__HRL).
