swirl_options(swirl_logging = FALSE)
library(dplyr)
purchases <- structure(list(MPEP_self = structure(c(7, 9, 9, 6, 7, 3, 9, 9, 
                                                    9, 8, 5, 8, 9, 9, 7, 7, 8, 9, 9, 5, 5, 3, 6, 7, 5, 8, 5, 7, 9, 
                                                    5, 7, 7, 1, 9, 6, 5, 9, 7, 8, 6, 7, 4, 7, 8, 7, 5, 2, 7, 7, 8, 
                                                    9, 9, 7, 7, 4, 4, 8, 8, 6, 9, 7, 5, 6, 9, 9, 9, 8, 6, 1, 7, 9, 
                                                    3, 7, 6, 8, 5, 3, 8, 7, 9, 8, 9, 5, 9, 1, 7, 9, 2, 5, 8, 9, 8, 
                                                    5, 9, 7, 9, 8, 6, 8, 8, 4, 6, 6, 1, 7, 9, 4, 9, 9, 4, 3, 5, 9, 
                                                    9, 8, 5, 7, 7, 5, 4, 8, 9, 7, 6, 6, 7, 3, 6, 7, 5, 7, 6, 8, 3, 
                                                    2, 3, 8, 8, 7, 6, 2, 4, 7, 6, 9, 8, 8, 9, 7, 3, 9, 6, 9, 9, 7, 
                                                    9, 5, 9, 9, 7, 5, 8, 9, 8, 5, 8, 9, 8, 8, 5, 9, 7, 8, 7, 5, 4, 
                                                    1, 7, 7, 9, 8, 2, 4, 3, 8, 4, 3, 9, 9, 5, 7, 4, 8, 5, 5, 6, 6, 
                                                    5, 7, 9, 8, 2, 5, 9, 3, 9, 7, 6, 6, 6, 9, 2, 7, 5, 8, 9, 7, 9, 
                                                    3, 3, 7, 6, 7, 7, 9, 2, 8, 2, 9, 9, 6, 5, 5, 4, 9, 4, 3, 9, 9, 
                                                    7, 9, 9, 4, 8, 5, 7, 9, 8, 8, 5, 7, 5, 8, 8, 6, 6, 5, 4, 6, 4, 
                                                    9, 8, 1, 9, 7, 7, 7, 7, 7, 8, 3, 9, 9, 3, 5, 7, 5, 7, 5, 8, 6, 
                                                    3, 6, 6, 9, 5, 9, 7, 2, 4, 5, 9, 1, 7, 9, 5, 9, 9, 7, 1, 9, 7, 
                                                    5, 7, 5, 7, 9, 9, 7, 1, 9, 8, 5, 5, 9, 2, 9, 7, 7, 9, 7, 5, 7, 
                                                    7, 9, 3, 7, 8, 9, 9, 3, 1, 6, 3, 7, 8, 9, 4, 5, 9, 4, 9, 9, 8, 
                                                    9, 8, 7, 5, 8, 9, 9, 9, 4, 4, 8, 9, 9, 9, 5, 9, 7, 9, 7, 9, 7, 
                                                    8, 5, 8, 8, 7, 7, 8, 3, 9, 7, 4, 5, 8, 6, 8, 6, 5, 6, 8, 7, 1, 
                                                    5, 6, 9, 8, 7, 7, 5, 5, 5, 8, 7, 8, 8, 5, 1, 8, 3, 5, 8, 3, 8, 
                                                    6, 8, 9, 9, 1, 3, 9, 8, 8, 6, 3, 8, 6, 9, 2, 9, 6, 7, 7, 5, 3, 
                                                    2, 7, 5, 8, 8, 7, 5, 2, 7, 5, 8, 7, 7, 9, 9, 7, 4, 3, 8, 7, 1, 
                                                    7, 6, 1, 7, 8, 5, 4, 7, 9, 9, 7, 8, 6, 8, 7, 7, 6, 7, 9, 9, 9, 
                                                    6, 6, 7, 5, 9, 9, 7, 7, 7, 7, 5, 3, 1, 9, 4, 6, 5, 9, 9, 9, 4, 
                                                    5, 1, 4, 3, 9, 4, 9, 3, 5, 8, 7, 6, 8, 5, 7, 5, 7, 5, 7, 9, 8, 
                                                    8, 6, 6, 9, 6, 9, 8, 5, 8, 5, 9, 9, 2, 8, 3, 5, 7, 7, 7, 7, 5, 
                                                    8, 7, 6, 5, 7, 3, 9, 1, 6, 9, 8, 7, 6, 5, 3, 8, 3, 9, 9, 7, 9, 
                                                    7, 7, 7, 4, 7, 6, 9, 7, 2, 6, 4, 7, 9, 1, 9, 9, 7, 8, 7, 8, 6, 
                                                    9, 5, 7, 6, 8, 5, 9, 4, 5, 5, 1, 7, 7, 4, 7, 9, 2, 5, 7, 2, 9, 
                                                    6, 7, 6, 8, 8, 6, 8, 8, 7, 7, 4, 3, 1, 9, 8, 4, 4, 9, 9, 9, 7, 
                                                    1, 4, 7, 5, 7, 6, 9, 9, 9, 9, 6, 3, 4, 9, 3, 5, 6, 7, 9, 9, 4, 
                                                    5, 7, 8, 8, 8, 6, 7, 7, 1, 4, 4, 2, 9, 7, 3, 7, 8, 8, 3, 9, 7, 
                                                    8, 9, 7, 9, 7, 6, 9, 7, 8, 9, 4, 7, 9, 5, 1, 7, 2, 3, 6, 7, 5, 
                                                    5, 6, 6, 8, 9, 6, 4, 6, 6, 5, 9, 5, 9, 7, 6, 5, 9, 7, 3, 6, 9, 
                                                    5, 1, 5, 9, 8, 6, 7, 5, 8, 9, 3, 7, 5, 1, 9, 6, 4, 5, 9, 2, 8, 
                                                    7, 7, 8, 9, 7, 4, 6, 7, 7, 8, 1, 5, 3, 5, 3, 6, 9, 2, 7, 9, 5
), display_width = 5L)), row.names = c(NA, -743L), class = c("tbl_df", 
                                                             "tbl", "data.frame"))

violence <- 
  structure(list(risk_group = c("Low-risk group", "Low-risk group", 
                                "Low-risk group", "Low-risk group", "Low-risk group", "Low-risk group", 
                                "Low-risk group", "Low-risk group", "Low-risk group", "Low-risk group", 
                                "Low-risk group", "Low-risk group", "Low-risk group", "Low-risk group", 
                                "Low-risk group", "Low-risk group", "Low-risk group", "Low-risk group", 
                                "Low-risk group", "Low-risk group", "Low-risk group", "Low-risk group", 
                                "Low-risk group", "Low-risk group", "Low-risk group", "Low-risk group", 
                                "Low-risk group", "Low-risk group", "Low-risk group", "Low-risk group", 
                                "Low-risk group", "Low-risk group", "Low-risk group", "Low-risk group", 
                                "Low-risk group", "Low-risk group", "Low-risk group", "Low-risk group", 
                                "Low-risk group", "High-risk group", "High-risk group", "High-risk group", 
                                "High-risk group", "High-risk group", "Low-risk group", "Low-risk group", 
                                "Low-risk group", "Low-risk group", "Low-risk group", "Low-risk group", 
                                "Low-risk group", "Low-risk group", "Low-risk group", "Low-risk group", 
                                "Low-risk group", "Low-risk group", "High-risk group", "High-risk group", 
                                "High-risk group", "Low-risk group", "Low-risk group", "Low-risk group", 
                                "Low-risk group", "Low-risk group", "Low-risk group", "Low-risk group", 
                                "Low-risk group", "Low-risk group", "Low-risk group", "Low-risk group", 
                                "Low-risk group", "Low-risk group", "High-risk group", "High-risk group", 
                                "High-risk group", "High-risk group", "High-risk group", "High-risk group", 
                                "High-risk group", "High-risk group", "High-risk group", "High-risk group", 
                                "High-risk group", "High-risk group", "High-risk group", "High-risk group", 
                                "High-risk group", "High-risk group", "High-risk group", "High-risk group", 
                                "High-risk group", "High-risk group", "High-risk group", "High-risk group", 
                                "High-risk group", "High-risk group", "High-risk group", "High-risk group", 
                                "High-risk group", "High-risk group", "High-risk group", "High-risk group", 
                                "High-risk group", "High-risk group", "High-risk group", "High-risk group", 
                                "High-risk group", "High-risk group", "High-risk group", "High-risk group", 
                                "High-risk group", "High-risk group", "High-risk group", "High-risk group", 
                                "High-risk group", "High-risk group", "High-risk group", "High-risk group", 
                                "High-risk group", "High-risk group", "High-risk group", "High-risk group", 
                                "High-risk group", "High-risk group", "High-risk group", "High-risk group", 
                                "High-risk group"),
                 YRB_dev = c(2, 1, 1, 1, 1, 1.5, 1, 1, 1, 
                             1, 1.5, 1, 1, 1, 1.5, 1.5, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1.5, 1.5, 
                             1, 1.5, 1, 1, 1, 2, 1, 1, 1.5, 1.5, 1, 1, 1, 1, 1, 1.5, 1.5, 
                             1, 1.5, 1.5, 1.5, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1.5, 1, 1, 2.5, 
                             1.5, 1, 1, 1, 1, 1, 1, 1.5, 1, 1, 2.5, 2, 1, 1, 1.5, 1, 1, 1, 
                             1, 1, 1, 1, 3.5, 1, 1, 1, 1, 1, 1.5, 1, 2, 2, 1, 3, 3, 2, 1, 
                             1, 2, 1.5, 1, 1.5, 1, 1, 2, 1, 1, 1, 2.5, 1, 3.5, 3.5, 1.5, 2.5, 
                             2.5, 2, 1, 4, 1, 1, 3.5, 1, 2.5, 1.5, 1),
                 YRB_current = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                 1, 1, 1, 1, 1.5, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                 1, 1, 1, 1, 1, 1, 1.5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                 1, 1.5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1.5, 1.5, 1, 1, 1, 1, 1, 
                                 1, 1, 1, 1.5, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1.5, 1, 
                                 1, 1, 1, 1.5, 1, 1),
                 poverty_dev = c(5.75, 7, 6, 6.5, 6, 6.75, 
                                 7, 6, 6.5, 7, 7, 6.5, 6.5, 6, 6, 6.75, 7, 6.5, 6.5, 6.75, 7, 
                                 7, 7, 5.75, 5.75, 6, 5.75, 6, 5, 5.5, 6, 6, 6, 7, 5.25, 5.75, 
                                 6, 6.5, 7, 6, 3.75, 7, 4.5, 7, 7, 7, 6, 7, 7, 7, 7, 6, 6, 5.75, 
                                 7, 7, 6, 7, 5.75, 7, 7, 3, 6.25, 7, 7, 7, 6, 7, 7, 7, 6, 6, 7, 
                                 3.75, 6, 6, 6.5, 7, 5.75, 6, 5.75, 6.5, 6, 1, 3, 6.25, 6.75, 
                                 1, 1.75, 7, 7, 6, 3, 5.25, 7, 4, 3.75, 6, 4.75, 6, 5.75, 6, 4, 
                                 3.25, 4, 6, 6, 4.5, 6, 5, 4.25, 4, 6, 2, 5.5, 6.5, 6, 6, 6, 5.5, 
                                 7, 7, 7, 5.75, 6, 7, 6),
                 poverty_current = c(3.75, 6, 5.75, 6.25, 5.75, 7, 6, 3.75, 5.25, 6.75, 5, 4.75, 4.5, 4.25, 5, 3.25, 7, 
                                     5.75, 6.25, 5.25, 4, 7, 6, 5.75, 5.75, 6, 4.5, 5.25, 5.25, 4.25, 
                                     6, 6, 5.75, 5.75, 6.25, 5.25, 6, 7, 7, 5.5, 3, 2.75, 4, 4, 7, 
                                     6, 5.25, 7, 5.75, 6.25, 5.25, 6, 5.75, 6, 5.25, 6.5, 4.5, 3.75, 
                                     4.75, 7, 6, 3, 7, 2.75, 4.5, 5.75, 5.25, 7, 6.25, 5.5, 6, 5, 
                                     1.75, 3.75, 6, 6, 5.75, 7, 5.25, 5.5, 5.25, 5, 5.75, 2.5, 5, 
                                     6, 5.75, 2.25, 1, 3, 3, 6, 3, 5.25, 3.5, 6, 2.75, 5.5, 4, 2.75, 
                                     2.25, 3.25, 4.5, 1, 4.25, 2, 2, 6, 2.75, 5, 5, 3.75, 4.75, 4.75, 
                                     5, 6.25, 6.75, 6.5, 3, 2.5, 7, 2.75, 2, 1.5, 4.25, 7, 2.75), 
                 dprime = c(0.282216147062508, 1.24963771316421, 0.871671944912287, 
                            1.55687736395148, 0, 0.30723965078727, 0.871671944912287, 
                            0.564432294125016, 0, -0.967421566101701, 0, 0.282216147062508, 
                            0, 0.30723965078727, 0.282216147062508, 0.871671944912287, 
                            -1.00376302017327, 0.30723965078727, 0.871671944912287, -1.00376302017327, 
                            0.564432294125016, 0.589455797849779, 0.282216147062508, 
                            0.282216147062508, 0, 0, 0, 0.871671944912287, -0.282216147062508, 
                            -0.282216147062508, 0, 0.30723965078727, 0, 0, -0.282216147062508, 
                            0.564432294125016, 0, 0.282216147062508, -0.589455797849779, 
                            0.282216147062508, 0, 0.282216147062508, 0.685205419039193, 
                            0.589455797849779, 0.871671944912287, 0, 0.589455797849779, 
                            0.564432294125016, -0.282216147062508, -1.00376302017327, 
                            0, 1.24963771316421, 0.967421566101701, 0.625797251921349, 
                            0, -0.589455797849779, -0.282216147062508, 0.589455797849779, 
                            0.589455797849779, 0.871671944912287, 0.282216147062508, 
                            0.871671944912287, 0.871671944912287, -0.377965768251923, 
                            0, 0.967421566101701, 0.685205419039193, 1.24963771316421, 
                            -0.282216147062508, 0.282216147062508, 0, 0, -0.967421566101701, 
                            0.967421566101701, 0, 0.564432294125016, -0.871671944912287, 
                            1.17891159569956, 0, -0.967421566101701, -0.871671944912287, 
                            0.564432294125016, 0, -1.24963771316421, 0.589455797849779, 
                            -0.625797251921349, 0.564432294125016, 0.30723965078727, 
                            0, 1.00376302017327, 0.589455797849779, 0.589455797849779, 
                            0.282216147062508, 0.282216147062508, 0, 0, 0, -0.589455797849779, 
                            0, 0.282216147062508, 0.685205419039193, 0.282216147062508, 
                            -0.871671944912287, 0.282216147062508, -0.564432294125016, 
                            0.685205419039193, -0.30723965078727, 0.564432294125016, 
                            0.30723965078727, -0.30723965078727, 1.24963771316421, 1.55687736395148, 
                            0.564432294125016, 0.282216147062508, 0, 0.30723965078727, 
                            1.17891159569956, 0, -1.24963771316421, -0.377965768251923, 
                            -0.282216147062508, 0, 0.377965768251923, 0, 0, 0.871671944912287, 
                            0.564432294125016),
                 C = c(-0.141108073531254, -0.342602709519597, -0.153619825393635, -0.188982884125961, 0, -0.435835972456143, 
                       -0.153619825393635, -5.55111512312578e-17, 0, 0.483710783050851, 
                       0.282216147062508, 0.141108073531254, 0, 0.435835972456143, 
                       0.141108073531254, 0.153619825393635, 1.09133730793641, 0.435835972456143, 
                       -0.153619825393635, 1.09133730793641, -5.55111512312578e-17, 
                       -0.294727898924889, -0.141108073531254, -0.141108073531254, 
                       0.282216147062508, 0.589455797849779, 0.589455797849779, 
                       0.153619825393635, 0.141108073531254, 0.141108073531254, 
                       0.282216147062508, 0.435835972456143, -0.589455797849779, 
                       0, 0.141108073531254, -5.55111512312578e-17, 0, 0.141108073531254, 
                       0.294727898924889, 0.141108073531254, -0.589455797849779, 
                       0.141108073531254, -0.624818856582105, -0.294727898924889, 
                       -0.153619825393635, 0, 0.294727898924889, -5.55111512312578e-17, 
                       -0.141108073531254, 1.09133730793641, -0.282216147062508, 
                       -0.342602709519597, 0.483710783050851, 1.28032019206238, 
                       0.282216147062508, 0.294727898924889, 0.141108073531254, 
                       -0.294727898924889, 0.294727898924889, 0.153619825393635, 
                       0.141108073531254, -0.153619825393635, 0.153619825393635, 
                       0.77843868197574, 0.282216147062508, 0.483710783050851, 0.624818856582105, 
                       0.342602709519597, 0.141108073531254, 0.141108073531254, 
                       0, 0, -0.483710783050851, -0.483710783050851, 0.282216147062508, 
                       -5.55111512312578e-17, 0.153619825393635, 0, 0, 0.483710783050851, 
                       0.153619825393635, -5.55111512312578e-17, 0, -0.342602709519597, 
                       -0.294727898924889, 1.28032019206238, -5.55111512312578e-17, 
                       0.435835972456143, 1.59321881802305, 1.09133730793641, -0.294727898924889, 
                       -0.294727898924889, 0.141108073531254, 0.141108073531254, 
                       0, 0.282216147062508, 0.282216147062508, 0.294727898924889, 
                       0.589455797849779, 0.141108073531254, -0.624818856582105, 
                       -0.141108073531254, 0.153619825393635, 0.141108073531254, 
                       -5.55111512312578e-17, -0.624818856582105, 0.435835972456143, 
                       -5.55111512312578e-17, 0.435835972456143, 0.435835972456143, 
                       0.342602709519597, -0.188982884125961, -5.55111512312578e-17, 
                       0.141108073531254, 0.282216147062508, -0.435835972456143, 
                       0, 0.589455797849779, 0.342602709519597, -0.77843868197574, 
                       -0.141108073531254, -0.282216147062508, -0.77843868197574, 
                       -0.282216147062508, 0, -0.153619825393635, -5.55111512312578e-17
                 )), 
            row.names = c(1L, 17L, 33L, 49L, 65L, 81L, 97L, 113L, 
                          129L, 145L, 161L, 177L, 193L, 209L, 225L, 241L, 257L, 273L, 289L, 
                          305L, 321L, 337L, 353L, 369L, 385L, 401L, 417L, 433L, 449L, 465L, 
                          481L, 497L, 513L, 529L, 545L, 561L, 577L, 593L, 609L, 625L, 641L, 
                          657L, 673L, 689L, 705L, 721L, 737L, 753L, 769L, 785L, 801L, 817L, 
                          833L, 849L, 865L, 881L, 897L, 913L, 929L, 945L, 961L, 977L, 993L, 
                          1009L, 1025L, 1041L, 1057L, 1073L, 1089L, 1105L, 1121L, 1137L, 
                          1153L, 1169L, 1185L, 1201L, 1217L, 1233L, 1249L, 1265L, 1281L, 
                          1297L, 1313L, 1329L, 1345L, 1361L, 1377L, 1393L, 1409L, 1425L, 
                          1441L, 1457L, 1473L, 1489L, 1505L, 1521L, 1537L, 1553L, 1569L, 
                          1585L, 1601L, 1617L, 1633L, 1649L, 1665L, 1681L, 1697L, 1713L, 
                          1729L, 1745L, 1761L, 1777L, 1793L, 1809L, 1825L, 1841L, 1857L, 
                          1873L, 1889L, 1905L, 1921L, 1937L, 1953L, 1969L, 1985L, 2001L, 
                          2017L), class = "data.frame",
            reshapeWide = list(v.names = c("response", "correct", "latency"),
                               timevar = "trial", idvar = "subject", 
                               times = 1:16, varying = structure(c("response.1", "correct.1", 
                                                                   "latency.1", "response.2", "correct.2", "latency.2", "response.3", 
                                                                   "correct.3", "latency.3", "response.4", "correct.4", "latency.4", 
                                                                   "response.5", "correct.5", "latency.5", "response.6", "correct.6", 
                                                                   "latency.6", "response.7", "correct.7", "latency.7", "response.8", 
                                                                   "correct.8", "latency.8", "response.9", "correct.9", "latency.9", 
                                                                   "response.10", "correct.10", "latency.10", "response.11", 
                                                                   "correct.11", "latency.11", "response.12", "correct.12", 
                                                                   "latency.12", "response.13", "correct.13", "latency.13", 
                                                                   "response.14", "correct.14", "latency.14", "response.15", 
                                                                   "correct.15", "latency.15", "response.16", "correct.16", 
                                                                   "latency.16"),
                                                                 dim = c(3L, 16L))))
