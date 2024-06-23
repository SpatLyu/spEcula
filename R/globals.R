# see https://forum.posit.co/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
# or https://forum.posit.co/t/when-programming-with-dplyr-what-is-the-correct-way-to-avoid-undefined-global-variables/55946

utils::globalVariables(c(".", "Nh", "Nr", "Nrh", "geometry", "rmse", "s_yr_bar",
                         "sampling_id", "sh2", "v_yh_bar", "v_yr_bar", "yh_bar",
                         "yr_bar"))
