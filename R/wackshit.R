# 
# reactive({
#   if (!is.null(kinlab::actual_spouse(input$id_mother1, evdat=paste(kinlab::as_date(kh.data::kh_mat[paste(input$id_mother1), ifelse(input$event_choice=="birthday", 1, 
#                                                                    ifelse(input$event_choice=="death",
#                                                                           dim(kh.data::kh_mat)[[2]], 2)),1])), kh.data::kh_ind, kh.data::kh_fam)) & ! kinlab::get_kinloc(kinlab::actual_spouse(1570, evdat=as.Date("1799-12-31"), kh.data::kh_ind, kh.data::kh_fam), "1799-12-31", evmat = kh.data::kh_mat) < 34){
renderPlot({ 
  kinlab::plot_kinmap(id=as.numeric(kinlab::actual_spouse(input$id_mother1, evdat=paste(kinlab::as_date(kh.data::kh_mat[paste(input$id_mother1), ifelse(input$event_choice=="birthday", 1, 
                                                                                                                                                        ifelse(input$event_choice=="death",
                                                                                                                                                               dim(kh.data::kh_mat)[[2]], 2)),1])), kh.data::kh_ind, kh.data::kh_fam)),
                      evdat=paste(kinlab::as_date(kh.data::kh_mat[paste(input$id_mother1), ifelse(input$event_choice=="birthday", 1, 
                                                                                                  ifelse(input$event_choice=="death",
                                                                                                         dim(kh.data::kh_mat)[[2]], 2)),1])),
                      list_kin=kh.data::kh_kin,
                      list_geo=kh.data::kh_geo,
                      my_map=kh.data::kh_geo[[2]][[1]],
                      spit_results=FALSE,
                      throw_plots=TRUE)
})
# }
# })

