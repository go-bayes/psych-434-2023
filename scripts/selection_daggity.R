# test daggity

library(dagitty)
library(ggdag)
g1 <- dagitty( "dag {
    A -> D
    A -> Y
    L-> Y
}")

g2 <- dagitty( "dag {
    Y <- X <- Z1 <- V -> Z2 -> Y
    Z1 <- W1 <-> W2 -> Z2
    X <- W1 -> Y
    X <- W2 -> Y
}")

plot(graphLayout(g1))

print( impliedConditionalIndependencies( g1 ) )


print( adjustmentSets( g1, "A", "Y") )


for( n in names(g1) ){
  for( m in setdiff( descendants( g1, n ), n ) ){
    a <- adjustmentSets( g1, n, m )
    if( length(a) > 0 ){
      cat("The total effect of ",n," on ",m,
          " is identifiable controlling for:\n",sep="")
      print( a, prefix=" * " )
    }
  }
}


# selection bias

#2

coords <- tibble::tribble(~ name, ~ x,  ~ y,
                          "L", 1,   2,
                          "D", 2,   3,
                          "A", 3,   2,
                          "Y", 4,   1)

dagify(Y ~ L,
       D ~ A,
       Y ~ L, coords = coords
) %>%
  ggdag_dseparated("Y", "A",
                   controlling_for = "D",
                   collider_lines = TRUE
  ) + theme_dag_grey()



#3
coords <- tibble::tribble(~ name, ~ x,  ~ y,
                          "L", 1,   2,
                          "D", 2,   3,
                          "A", 3,   2,
                          "Y", 4,   1)

dag <- dagify(Y ~ L,
       D ~ L + A,
       exposure = "A",
       outcome = "Y",
       coords = coords
) %>%
  control_for(c("D","L"))
  # ggdag_dseparated("Y", "A",
  #                  controlling_for = "D",
  #                  collider_lines = TRUE
  # ) + theme_dag_grey()

ggdag_adjust(dag,collider_lines = TRUE) + theme_dag_blank()




ggdag_adjust(dag,collider_lines = FALSE) + theme_dag_blank()

ggdag_dseparated(dag, "Y", "A",
                 controlling_for = "D",
                 collider_lines = TRUE
) + theme_dag_grey()




#4 colider bias
coords <- tibble::tribble(~ name, ~ x,  ~ y,
                          "L", 1,   2,
                          "D", 2,   3,
                          "A", 3,   2,
                          "Y", 4,   1)

dagify(Y ~ L,
       D ~ A + L,  coords = coords
) %>%
  ggdag_dseparated("Y", "A",
                   controlling_for = c("D","L"),
                   collider_lines = T
  ) + theme_dag_grey()



#5
coords <- tibble::tribble(~ name, ~ x,  ~ y,
                          "Q", 0, 0,
                          "Z", 0, 1,
                          "L", 1,   2,
                          "D", 2,   3,
                          "A", 3,   2,
                          "Y", 4,   1)

dagify(Y ~ Z,
       A ~ Q,
       L ~ Q + Z,
       D ~ A + L,  coords = coords
) %>%
  ggdag_dseparated("Y", "A",
                   controlling_for = "D",
                   collider_lines = T,
  ) + theme_dag_grey()






