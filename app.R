library(shiny)
library(tidyverse)
library(bslib)
library(htmltools)
library(shinyWidgets)


# set colors for pitches
# this will allow us to have custom and standard colors for all pitch types
pitches = c("Fastball", 
            "Four-Seam", 
            "Sinker", 
            "Slider", 
            "Sweeper", 
            "Curveball", 
            "Changeup", 
            "Splitter", 
            "Cutter")

pitch_colors = c("Fastball" = '#d7191c', 
                 "Four-Seam" = '#d7191c', 
                 "Sinker" = "#fdae61", 
                 "Slider" = "#A020F0", 
                 "Sweeper" = "magenta",
                 "Curveball" = '#2c7bb6', 
                 "Changeup" = '#90EE90',
                 "Splitter" = '#90EE32',
                 "Cutter" = "pink")

df <- # delete this text with a function to read in your data
      
df <- df %>% 
      # I like to filter out pitches where the trackman wasn't operating, 
      # this may not be necessary if you do na.rm = T in later operations but I like to be safe
      filter(is.na(HorzBreak) == F) %>% 
      mutate(
            # this is where I create custom tagging conditions
            # I find the best variables for this are speed, break, and spin axis. 
            # Spin rate is also especially useful for offspeed pitches
            PitchType = case_when(
                  Pitcher == "Appelbaum, Gabe" & RelSpeed > 65 ~ "Fastball",
                  Pitcher == "Appelbaum, Gabe" & RelSpeed < 65 ~ "Slider"
            ),
            
            # custom tagging conditions are super time intensive, it takes a lot of work and maintenance to write custom conditions for each pitcher's individual pitches
            # I firmly believe that this extra work is worth it and makes a big difference in the effectiveness and presentation of the app
            # however if you don't have the time for that you can comment out the above section and use the below instead
            # PitchType = TaggedPitchType # you can also do PitchType = AutoPitchType
            
            # this line orders the pitches by the custom order set above
            PitchType = factor(PitchType, levels = pitches),
            
            # this line changes the name's from being Last, First to First Last
            Pitcher = str_replace_all(Pitcher, "(\\w+), (\\w+)", "\\2 \\1"),
            
            # this create's a dummy variable for whether or not the pitch is in the zone
            # this will be used for some metric calculation later
            # it is not a true zone. it is based on the average height and dimensions of players
            inZone = case_when(
                  between(PlateLocHeight, 1.6, 3.4) & between(PlateLocSide, -0.71, 0.71) ~ 1,
                  T ~ 0
            ),
            
            # self explanatory, dummy variable for any pitch that is out of the zone and swung at
            Chase = case_when(
                  inZone == 0 & PitchCall %in% c('FoulBall', 
                                                 'FoulBallNotFieldable',
                                                 'InPlay',
                                                 'StrikeSwinging') ~ 1,
                  T ~ 0
            ),
            
            # This line creates a more digestible game ID for selecting certain game reports instead of an overall report
            CustomGameID = paste0(
                  #substr(Date, 7, 10), if you want to have just the month and day instead of year-month-day uncomment this line and and comment the next line out
                  Date,
                  ": ",
                  str_sub(AwayTeam, 1,3),
                  " @ ",
                  str_sub(HomeTeam, 1,3)
            )
      )

# add home plate segments, this will be used to draw a homeplate on certain plots later
home_plate_segments <- data.frame(
      x = c(0, 0.71, 0.71, 0, -0.71, -0.71),
      y = c(0.15, 0.15, 0.3, 0.5, 0.3, 0.15),
      xend = c(0.71, 0.71, 0, -0.71, -0.71, 0),
      yend = c(0.15, 0.3, 0.5, 0.3, 0.15, 0.15)
)

# UI ####

# this is where the interface is set up
ui <- page_sidebar( # this makes the interface have a sidebar so you can minimize the selection window
      title = "Title this whatever you want",
      sidebar = sidebar(
            title = "Select Pitcher/Game",
            # this creates a pitcher selection input
            selectInput(
                  inputId = "PitcherInput", 
                  label = "Select Pitcher", 
                  choices = sort(unique(df$Pitcher)),
                  selectize = T
            ),
            # game selection input
            pickerInput(
                  inputId = "GameInput",
                  label = HTML("Select Game<br>(Selects all by default)"),
                  choices = unique(df$CustomGameID),
                  options = list(`actions-box` = TRUE),
                  multiple = T
            ),
            # batter hand input, can do both or just one
            pickerInput(
                  inputId = "BatterHand",
                  label = HTML("Select Batter Hand<br>(Selects both by default)"),
                  choices = unique(df$BatterSide),
                  selected = unique(df$BatterSide),
                  options = list(`actions-box` = TRUE),
                  multiple = T
            )
      ),
      # this sets the tabs at the top
      # you can title the plot and the output function, either plotOutput or tableOutput tells the code what object to render in that tab
      # there are several other ways to render besides plots and tables, there are also interactive tables as well available
      navset_tab(
            nav_panel(title = "Movement & Metrics", plotOutput("movement"), tableOutput("metrics")),
            nav_panel(title = "Clock", plotOutput("clock")),
            nav_panel(title = "Velo Distribution", plotOutput("velo_density")),
            nav_panel(title = "Locations", plotOutput("locations")),
            nav_panel(title = "Whiffs, Chase, Called Strikes", 
                      plotOutput("whiffs"), 
                      plotOutput("chase"), 
                      plotOutput("calledStrikes")
                      )
      )
)
#####

# this section is where the actual code is that generates the table and plots
server <- function(input, output, session){
      
      # this code changes the options for games to select based on what pitcher you selected
      # basically when you pick your pitcher this filters down the games available to ones that pitcher appeared in
      observeEvent(input$PitcherInput,

                   updatePickerInput(
                         session,
                         inputId = "GameInput",
                         choices = sort(unique(df$CustomGameID[df$Pitcher == input$PitcherInput])),
                         selected = sort(unique(df$CustomGameID[df$Pitcher == input$PitcherInput]))
                   )
                   
      )
      
      # code for the pitch movement plot
      output$movement <- renderPlot({
            
            # this filters the dataset down to just the selections you've made
            dataFilter <- reactive({
                  df %>%
                        filter(
                              Pitcher == input$PitcherInput,
                              CustomGameID %in% c(input$GameInput),
                              BatterSide %in% c(input$BatterHand)
                        )
            })
            
            # pitch movement plot itself
            dataFilter() %>%
                  ggplot(aes(HorzBreak, InducedVertBreak)) +
                  geom_point(aes(color = PitchType), na.rm = TRUE, alpha = .9, size = 2.5) + # make a scatterplot, alpha controls the opacity of the plot
                  scale_x_continuous(limits = c(-25, 25), breaks = seq(-25, 25, 5)) + # set limits of the scale and have ticks every 5
                  scale_y_continuous(limits = c(-25, 25), breaks = seq(-25, 25, 5)) +
                  scale_color_manual(values = pitch_colors) + # color the points by the pitch colors set earlier
                  geom_hline(yintercept = 0, linetype = 2) + # these two lines create dashed lines at the intercepts
                  geom_vline(xintercept = 0, linetype = 2) + 
                  theme_bw(base_size = 20) + # sets the plot theme, base_size should always be used! increases the size of text and other visual aids
                  labs( # labels
                        title = paste0(input$PitcherInput, ": Pitch Movement"), # add the pitchers name into the plot title
                        y = "Induced Vertical Break", 
                        x = "Horizontal Break", 
                        color = "Pitch Type" # this is the title of the legend
                  ) +
                  theme( # center the title, put the legend on the right of the graph and increase the text size of the legend
                        plot.title = element_text(hjust = 0.5, face = "bold", size = "20"),
                        legend.position = "right",
                        legend.text = element_text(size = 12)
                  ) +
                  coord_fixed() + # this makes it the so the plot will always have the same dimensions and won't shift around by each pitcher
                  guides(color = guide_legend(override.aes = list(size = 3))) # this line makes the dots in the legend larger so they're more readable
            
      })
      
      # this creates a spin clock so that you can see the direction and magnitude of spin
      output$clock <- renderPlot({
            
            # this filters the dataset down to just the selections you've made
            dataFilter <- reactive({
                  df %>%
                        filter(Pitcher == input$PitcherInput,
                               CustomGameID %in% c(input$GameInput),
                               BatterSide %in% c(input$BatterHand)
                               )
            })
            
            # this converts degrees of spin direction to tilt on a clock for labelling
            degree_to_clock <- function(degree) {
                  labels <- c("6:00", "7:00", "8:00", "9:00", "10:00", "11:00", "12:00", "1:00", "2:00", "3:00", "4:00", "5:00")
                  labels[((degree / 30) %% 12) + 1]
            }
            
            # labels along the clock and rings
            labels_df <- data.frame(
                  angle = c(100, 100, 100, 100),
                  radius = c(1000, 1500, 2000, 2500),
                  label = c("1000 rpm", "1500 rpm", "2000 rpm", "2500 rpm")
            )
            
            # spin tilt plot
            dataFilter () %>%
                  group_by(PitchType) %>%
                  summarise(
                        avgSpinAxis = mean(SpinAxis),
                        avgSpin = mean(SpinRate)
                  ) %>%
                  ggplot(aes(avgSpinAxis, avgSpin)) +
                  geom_col(aes(fill = PitchType), width = 5, color = "black") + # make columns
                  scale_fill_manual(values = pitch_colors) + # set colors
                  # big blob of text labeling \/
                  geom_text(data = labels_df, aes(x = angle, y = radius, label = label), color = "black", size = 3) +
                  coord_polar(start = pi) + # this changes from a typical 2d plot to a circular plot
                  # the other thing happening in the above line is start=pi, 
                  # that sets the starting point to the middle right because 0 degree spinaxis in trackman is equivalent to spin coming from the third base side
                  # but that isn't what coord_polar considers as the starting point by default so this line shifts the starting point to where trackman considers 0
                  scale_x_continuous(
                        limits = c(0, 360), # limit to 360 degrees because that's a circle
                        breaks = seq(0, 360, 30), # Set line breaks every 30 degrees
                        labels = degree_to_clock(seq(0, 360, 30)), # label using the degree_to_clock function, converting degrees to o'clock
                  ) +
                  theme_minimal(base_size = 15) + # set the theme and do base_size again, important!
                  theme(
                        # set the background to white
                        panel.background = element_rect(fill = "white", color = "white"),
                        # These next 3 lines remove or edit grid lines, 
                        # best way to understand them is to comment them out and see how it looks without them
                        panel.grid.minor = element_blank(), 
                        panel.grid.major.x = element_blank(),
                        panel.grid.major.y = element_line(color = "gray", size = 0.5),
                        axis.title =  element_blank() # remove the title of the plot
                  )
            
      })
      
      # this is the metrics table, shows averages by pitch type
      output$metrics <- renderTable({
            
            # this filters the dataset down to just the selections you've made
            dataFilter <- reactive({
                  df %>%
                        filter(Pitcher == input$PitcherInput,
                               CustomGameID %in% c(input$GameInput),
                               BatterSide %in% c(input$BatterHand)
                               )
            })
            
            dataFilter() %>%
                  # this filter keeps the table updated with what specific game and batter hand have been filtered for
                  # for some reason when the table is in the same tab as the movement plot it wasn't updating properly
                  # I thought this was supposed to auto update with the function above but it wasn't working so I added the filter below to be safe
                  filter(CustomGameID %in% c(input$GameInput), BatterSide %in% c(input$BatterHand)) %>%
                  group_by(PitchType) %>%
                  # this whole section just finds averages for each of these metrics by pitch type
                  summarize(
                        Total = n(),
                        Velo = round(mean(RelSpeed, na.rm = T), 1),
                        Max = round(max(RelSpeed, na.rm = T), 1),
                        iVB = round(mean(InducedVertBreak, na.rm = T), 1),
                        HB = round(mean(HorzBreak, na.rm = T), 1),
                        VAA = round(mean(VertApprAngle, na.rm = T), 1),
                        HAA = round(mean(HorzApprAngle, na.rm = T), 1),
                        Spin = round(mean(SpinRate, na.rm = T)),
                        `Height (rel)` = round(mean(RelHeight), 1),
                        `Side (rel)` = round(mean(RelSide), 1),
                        Extension = round(mean(Extension, na.rm = T), 1),
                        # whiff% is percent of swings where the batter misses entirely
                        # that section at the end is all the ways trackman describes a swing
                        `Whiff%` = paste0(round(sum(PitchCall == "StrikeSwinging") / sum(PitchCall %in% c("StrikeSwinging",
                                                                                                          "InPlay",
                                                                                                          "FoulBallFieldable",
                                                                                                          "FoulBallNotFieldable")) * 100), "%"),
                        # CSW stands for called strikes plus whiffs. Invented by PitcherList and they have a great article on it
                        # It's the percent of pitches thrown that result in whiffs or called strikes
                        # It's strike percentage without fouls or balls in play so I like to think of it as non-competitive strikes
                        `CSW%` = paste0(round(sum(PitchCall %in% c("StrikeSwinging", "StrikeCalled")) / n() * 100), "%")
                  )
      }, digits = 1) # this digits bit at the end makes sure that every metric rounds to one decimal point
      
      # density plot of velocity. shows where you're sitting and what the spread is between max and slowest pitches
      output$velo_density <- renderPlot({
            
            dataFilter <- reactive({
                  df %>%
                        filter(Pitcher == input$PitcherInput,
                               CustomGameID %in% c(input$GameInput),
                               BatterSide %in% c(input$BatterHand)
                               )
            })
            
            # find the average velo so it can be labeled in the density plot
            mean_velo <- dataFilter() %>%
                  group_by(PitchType) %>%
                  summarise(mean_speed = round(mean(RelSpeed, na.rm = TRUE), 1))
            
            dataFilter() %>%
                  ggplot(aes(RelSpeed, color = PitchType, fill = PitchType)) +
                  # i used color and fill so that the line along the top and the density itself could both be colored in
                  scale_color_manual(values = pitch_colors) +
                  scale_fill_manual(values = pitch_colors) +
                  # I did 2 geom_density's so that the line on top of the density could be fully opaque and the density istelf could be a bit see through
                  geom_density(alpha = 1, fill = NA, show.legend = FALSE, trim = T) +
                  geom_density(alpha = 0.4, color = NA, show.legend = FALSE, trim = T) +
                  # facet_wrap basically gives you the same plot but for a few different groups
                  # ncol sets how many columns will be in the matrix of plots
                  # scales = "free_y" in this case removes the y axis which you dont need because you can visually see the density
                  # strip.postion sets where the labels are. in this case it sets the pitch type labels on the left of the plot
                  facet_wrap(~PitchType, ncol = 1, scales = "free_y", strip.position = "left") + 
                  geom_text( # a lot going on here but basically it takes the mean of velo that was calculated earlier and labels the density plot with the average velo
                        data = mean_velo, 
                        aes(
                              x = mean_speed, 
                              label = sprintf("%.2f", mean_speed)
                        ),
                        y = 0, 
                        vjust = -0.5, 
                        color = "black", 
                        size = 3
                  ) +
                  # this section tweaks the specific grids and such that show up and don't show up
                  # again best way to visualize all this is comment it in and out
                  theme(axis.ticks.y = element_blank(),
                        axis.line.y = element_blank(),
                        axis.text.y = element_blank(),
                        strip.text.y.left = element_text(angle = 0),
                        panel.grid.major.x = element_line(color = "gray", size = 0.1),
                        panel.grid.minor.y = element_blank(),
                        panel.background = element_rect(fill = "white"),
                        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
                  labs(y = NULL, x = "Pitch Velo") # plot labels
            
      })
      
      # this plot shows location by pitch type
      output$locations <- renderPlot({
            
            dataFilter <- reactive({
                  df %>%
                        filter(Pitcher == input$PitcherInput,
                               CustomGameID %in% c(input$GameInput),
                               BatterSide %in% c(input$BatterHand))
            })
            
            dataFilter() %>%
                  ggplot(aes(PlateLocSide, PlateLocHeight)) + 
                  geom_point(na.rm = TRUE) +
                  # this time facet wrap sets the columns to 4
                  # I did this so that most pitchers would have all their pitches on the same row
                  facet_wrap(~PitchType, ncol = 4) + 
                  # this creates a heatmap of locations, 
                  #raster is a type of spatial data representation that fills in areas between data points using information from area around those blank points
                  stat_density2d(geom = 'raster', aes(fill = after_stat(density)), contour = F, na.rm = TRUE) +
                  ylim(0,5) + 
                  xlim(-3, 3) +
                  # there are other ways to set the color scheme but this manual control was important to me to properly show the scale of how common certain locations are
                  # I also wanted to fade to white so that it fades into the background rather than having different color's represent uncommon locations vs no pitches in this location, those two things should be represented by the same color
                  scale_fill_gradientn(colours = c(
                        "#f7f7f7", 
                        "#fff7bc",
                        "#fee391",
                        "#fec44f",
                        "#fe9929",
                        "#ec7014",
                        "#cc4c02",
                        "#993404",
                        "#662506"
                        )) +
                  # this code below adds an estimated strike zone
                  annotate("rect", xmin = -1, xmax = 1,
                           ymin = 1.6, ymax = 3.4,
                           fill = NA, color = "black") +
                  # this adds plot titles and x and y axis titles
                  labs(title = paste0(input$PitcherInput, ": Pitch Location (pitcher's view)"), y = "Vertical Location", x = "Horizontal Location") + 
                  # this section tweaks the specific grids and such that show up and don't show up
                  # again best way to visualize all this is comment it in and out
                  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = "20"),
                        strip.background = element_rect(fill = "white", color = "black", linewidth = 2),
                        strip.text = element_text(size = 15),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
                        panel.background = element_rect(fill = "#f7f7f7")) +
                  # this code below removes the legend. By default the legend would show what frequency each color represents
                  # I felt it was unnecessary in this case, darker means more and lighter means less
                  guides(fill = "none") +
                  coord_fixed() +
                  geom_segment( # this draws in a plate, helps visualize the pov
                        data = home_plate_segments, 
                        aes(x = x, y = y, xend = xend, yend = yend),
                        color = "black"
                  )
      })
      
      # The following whiff, chase, and called strikes plots are all essentially the same and you can reference the earlier comments to understand specific lines
      # Generally they show the strike zone and where whiffs, chases, and called strikes occur. The plots are colored by pitch type
      
      output$whiffs <- renderPlot({
            
            dataFilter <- reactive({
                  df %>%
                        filter(Pitcher == input$PitcherInput,
                               CustomGameID %in% c(input$GameInput),
                               BatterSide %in% c(input$BatterHand)
                               )
            })
            
            dataFilter() %>%
                  filter(PitchCall == "StrikeSwinging") %>%
                  ggplot(aes(PlateLocSide, PlateLocHeight)) +
                  geom_point(aes(color = PitchType), na.rm = TRUE, size = 3, alpha = 0.85) +
                  scale_color_manual(values = pitch_colors) +
                  ylim(0,5) +
                  xlim(-3, 3) +
                  annotate("rect", xmin = -.71, xmax = .71,
                           ymin = 1.6, ymax = 3.4,
                           fill = NA, color = "black") +
                  labs(title = paste0(input$PitcherInput, ": Whiff's (pitcher's view)"), y = "Vertical Location", x = "Horizontal Location") +
                  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = "20"),
                        strip.background = element_rect(fill = "white", color = "black", linewidth = 2),
                        strip.text = element_text(size = 15),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
                        panel.background = element_rect(fill = "#f7f7f7")) +
                  guides(fill = "none") +
                  coord_fixed() +
                  geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                               color = "black")
      
      })
      
      output$chase <- renderPlot({
            
            dataFilter <- reactive({
                  df %>%
                        filter(Pitcher == input$PitcherInput,
                               CustomGameID %in% c(input$GameInput),
                               BatterSide %in% c(input$BatterHand)
                               )
            })
            
            dataFilter() %>%
                  filter(Chase == 1) %>%
                  ggplot(aes(PlateLocSide, PlateLocHeight)) +
                  geom_point(aes(color = PitchType), na.rm = TRUE, size = 3, alpha = 0.85) +
                  scale_color_manual(values = pitch_colors) +
                  ylim(0,5) +
                  xlim(-3, 3) +
                  annotate("rect", xmin = -.71, xmax = .71,
                           ymin = 1.6, ymax = 3.4,
                           fill = NA, color = "black") +
                  labs(title = paste0(input$PitcherInput, ": Chase's (pitcher's view)"), y = "Vertical Location", x = "Horizontal Location") +
                  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = "20"),
                        strip.background = element_rect(fill = "white", color = "black", linewidth = 2),
                        strip.text = element_text(size = 15),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
                        panel.background = element_rect(fill = "#f7f7f7")) +
                  guides(fill = "none") +
                  coord_fixed() +
                  geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                               color = "black")
      })
      
      output$calledStrikes <- renderPlot({
            
            dataFilter <- reactive({
                  df %>%
                        filter(Pitcher == input$PitcherInput,
                               CustomGameID %in% c(input$GameInput),
                               BatterSide %in% c(input$BatterHand)
                               )
            })
            
            dataFilter() %>%
                  filter(PitchCall == "StrikeCalled") %>%
                  ggplot(aes(PlateLocSide, PlateLocHeight)) +
                  geom_point(aes(color = PitchType), na.rm = TRUE, size = 3, alpha = 0.85) +
                  scale_color_manual(values = pitch_colors) +
                  ylim(0,5) +
                  xlim(-3, 3) +
                  annotate("rect", xmin = -.71, xmax = .71,
                           ymin = 1.6, ymax = 3.4,
                           fill = NA, color = "black") +
                  labs(title = paste0(input$PitcherInput, ": Called Strikes (pitcher's view)"), y = "Vertical Location", x = "Horizontal Location") +
                  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = "20"),
                        strip.background = element_rect(fill = "white", color = "black", linewidth = 2),
                        strip.text = element_text(size = 15),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
                        panel.background = element_rect(fill = "#f7f7f7")) +
                  guides(fill = "none") +
                  coord_fixed() +
                  geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                               color = "black")
      })
}

# this last line is what launches the app itself, it takes the two objects you created which hold the interface and the backend code and turn it into an app
shinyApp(ui, server)


