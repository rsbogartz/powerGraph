library(shiny)

shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
  M1 <- input$Mn
 SD <- input$Sd
 Effect.Size <- input$effectSize
 alpha <- input$alpha
 N <- input$N
   
M2 <- M1+Effect.Size;

z <- qnorm(1-alpha/2 , 0,1);

Ntext <- as.character(N);
#SHOW DISTRIBUTION OF MEANS UNDER THE NULL HYPOTHESIS;
curve(dnorm(x,M1,SD/N^.5), from = M1-10*SD/N^.5, to = M1+10*SD/N^.5, n = 300, col = "red", cex.axis = .8, lwd = 3, xlab = "Sample Mean", ylab = "Probability Density");
text(M1 - 3*z*SD/N^.5 , .06,  "Null hyp. Dist. of Means", col = "red" ,cex = .8);
#SHOW N AND SIGMA;
#text(M1 + 3*z*SD/N^.5 , .0225,  "N = ", cex = .8);
#text(M1 + 4*z*SD/N^.5 , .0225,  Ntext, , cex = .8);
#text(M1 + 3*z*SD/N^.5 , .0170,  "σ = ",  cex = .8);
#text(M1 + 4.0*z*SD/N^.5 , .0170,  as.character(SD),  cex = .8);
#SHOW THE CUTOFF VALUES;
lines(c(M1 + z*SD/N^.5, M1 + z*SD/N^.5), c(0,.045), lwd = 3);
lines(c(M1 - z*SD/N^.5, M1 - z*SD/N^.5), c(0,.045), lwd = 3);
#text(M1 + 3*z*SD/N^.5 , .04,  "Vertical black lines ",  cex = .8);
#text(M1 + 3*z*SD/N^.5 , .035,  "are cutoff values.",  cex = .8);
#SHOW THE REJECTION REGION;
#text(M1 - 3*z*SD/N^.5 , .03,  "Rejection region", col = "darkgoldenrod4" ,cex = .8);
#text(M1 - 3*z*SD/N^.5 , .03,  "Rejection region", col = "darkgoldenrod4" ,cex = .8);  
#text(M1 - 3*z*SD/N^.5-20 , .027,  "M ≤ ", col = "darkgoldenrod4" ,cex = .8); 
#text(M1 - 3*z*SD/N^.5 -12  , .027,  as.character(round(M1 - z*SD/N^.5,2)), col = "darkgoldenrod4" ,cex = .8);  
#text(M1 - 3*z*SD/N^.5  , .027,  "or   M ≥ ", col = "darkgoldenrod4" ,cex = .8); 
#text(M1 - 3*z*SD/N^.5  +10 , .027,  as.character(round(M1 + z*SD/N^.5,2)), col = "darkgoldenrod4" ,cex = .8);  
lines(c(M1 + z*SD/N^.5, 1000), c(0,0), col = "darkgoldenrod4",lwd = 5);
lines(c(M1 - z*SD/N^.5, -1000), c(0,0), col = "darkgoldenrod4",lwd = 5);

 Shade.in.normal.twotailed = function(lower.left1, lower.right1, lower.left2,lower.right2,mu, sd, color) { 
sequence1 =  seq(lower.left1*100, lower.right1*100, by = 1) / 100
sequence2 =  seq(lower.left2*100, lower.right2*100, by = 1) / 100 
polygon(c(lower.left1, sequence1, lower.right1)  ,                                  
c(0, dnorm(sequence1, mu, sd), 0), col = color)
polygon(c(lower.left2, sequence2, lower.right2)  ,                                  
c(0, dnorm(sequence2, mu, sd), 0), col = color,  density = 40)
}

#SHOW ALPHA, THE PROBABILITY OF FALSELY REJECTING A TRUE NULL HYPOTHESIS;
text(M2 + 3.2*SD/N^.5 , .048,  "alpha = ",  cex = .8, col = "red");
text(M2 + 3.2*SD/N^.5, .038,  as.character(alpha),  cex = .8, col = "red");
#SHADE IN POWER;
Shade.in.normal.twotailed(M1+z*SD/(N^.5), M2+ 4*SD/N^.5, M1+z*SD/(N^.5), M2+ 4*SD/N^.5, M1+ Effect.Size, SD/(N^.5), "blue")


#SHOW THE TRUE DISTRIBUTION OF MEANS;
curve(dnorm(x, M2, SD/N^.5), from =M2-10*SD/N^.5, to = M2+10*SD/N^.5, n = 300, , col = "blue",  lwd = 3,add = T);
text(M1 - 3*z*SD/N^.5 , .05,  "True Distribution of Means", col = "blue" ,cex = .8);

#SHOW THE NUMERICAL VALUE OF POWER;
power <- 1 - pnorm(M1 + z*SD/N^.5,M2,SD/N^.5) + pnorm(M1 - z*SD/N^.5, M2, SD/N^.5);
power;
powerchar <- as.character(round(power,3));
text(M1 - 3*z*SD/N^.5 , .019,  "Power = ", , cex = .8);
text(M1 - 3*z*SD/N^.5 , .0140,  powerchar, , cex = .8);
#SHOW THE EFFECT SIZE
text(M1 - 3*z*SD/N^.5 , .04,  "Effect size = ", col = "green" ,cex = .8);
text(M1 - 3*z*SD/N^.5 , .035,  "M2 - M1 = ", col = "green" ,cex = .8);
text(M1 - 3*z*SD/N^.5 , .03,  as.character(round(M2 - M1,2)), col = "green" ,cex = .8);
text((M1 + M2)/2 , .005,  as.character(round(M2 - M1,2)), col = "green" ,cex = .8);
lines(c(M1, M2), c(.002,.002), col = "green", lwd = 3);
#SHOW THE REJECTION REGION;
#text(M1 - 3*z*SD/N^.5 , .03,  "Rejection region", col = "darkgoldenrod4" ,cex = .8);  
lines(c(M1 + z*SD/N^.5, 1000), c(0,0), col = "darkgoldenrod4",lwd = 5);
lines(c(M1 - z*SD/N^.5, -1000), c(0,0), col = "darkgoldenrod4",lwd = 5);}



   
      
  )
  
})

