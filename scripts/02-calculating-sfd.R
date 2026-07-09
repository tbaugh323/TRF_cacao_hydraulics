#### Calculating SFD ####

tree_2_all <- read_csv("data/sap_flow/tree_2/tree_2_all.csv")
# remotes::install_github("the-Hull/TREX")
library(TREX)

# load raw data
raw <- is.trex(example.data(type="doy"),
               tz="GMT",
               time.format="%H:%M",
               solar.time=TRUE,
               long.deg=7.7459,
               ref.add=FALSE)

# adjust time steps
input <- dt.steps(input=raw, 
                  start="2013-05-01 00:00",
                  end="2013-11-01 00:00",
                  time.int=15,
                  max.gap=60,
                  decimals=10,
                  df=FALSE)

# remove obvious outliers
input[which(input<0.2)]<- NA

input <- tdm_dt.max(input,
                    methods = c("pd", "mw", "dr"),
                    det.pd = TRUE,
                    interpolate = FALSE,
                    max.days = 10,
                    df = FALSE)

plot(input$input, ylab = expression(Delta*italic("V")))

lines(input$max.pd, col = "green")
lines(input$max.mw, col = "blue")
lines(input$max.dr, col = "orange")

output.data<- tdm_cal.sfd(input,make.plot=TRUE,df=FALSE,wood="Coniferous")

plot(output.data$sfd.pd$sfd[1:1000, ], ylim=c(0,10))
# see estimated uncertainty
lines(output.data$sfd.pd$q025[1:1000, ], lty=1,col="grey")
lines(output.data$sfd.pd$q975[1:1000, ], lty=1,col="grey")
lines(output.data$sfd.pd$sfd[1:1000, ])

sfd_data <- output.data$sfd.dr$sfd
sfd_new <- aggregate(sfd_data, unique(zoo::index(output.data$sfd.dr$sfd)), mean)

sfd_new_hold <- data.frame(sfd_data)
sfd_new_hold <- rownames_to_column(sfd_new_hold, "dt") |> 
  mutate(dt = strftime(dt, format = "%Y-%m-%d %H:%M:%S"))

sfd_new <- is.trex(sfd_new_hold,
                   tz="GMT",
                   time.format="%Y-%m-%d %H:%M:%S",
                   solar.time=T,
                   long.deg=7.7459,
                   ref.add=FALSE,
                   df=FALSE)

output<- out.data(input=sfd_new,
                  vpd.input=vpd, 
                  sr.input=sr,
                  prec.input=preci,
                  low.sr = 150,
                  peak.sr=300, 
                  vpd.cutoff= 0.5, 
                  prec.lim=1,
                  method="env.filt", 
                  max.quant=0.99, 
                  make.plot=TRUE)
