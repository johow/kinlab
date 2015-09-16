library(kinlab)
context("genealogical linkage")

test_that("get_moms gives IDs of mothers", {
  expect_equal(get_moms(kh33::kh[["df_ped"]]$id[kh33::kh[["df_ped"]]$id==1067], kh33::kh[["df_ped"]]), kh33::kh[["df_ped"]]$momid[kh33::kh[["df_ped"]]$id==1067])
  expect_equal(get_moms(kh33::kh[["df_ped"]]$id[kh33::kh[["df_ped"]]$id==47499], kh33::kh[["df_ped"]]), kh33::kh[["df_ped"]]$momid[kh33::kh[["df_ped"]]$id==47499])
})

test_that("get_dads gives IDs of fathers", {
  expect_equal(get_dads(kh33::kh[["df_ped"]]$id[kh33::kh[["df_ped"]]$id==1067], kh33::kh[["df_ped"]]), kh33::kh[["df_ped"]]$dadid[kh33::kh[["df_ped"]]$id==1067])
  expect_equal(get_dads(kh33::kh[["df_ped"]]$id[kh33::kh[["df_ped"]]$id==47499], kh33::kh[["df_ped"]]), kh33::kh[["df_ped"]]$dadid[kh33::kh[["df_ped"]]$id==47499])
})
