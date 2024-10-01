library(shiny)
library(htmltools)

get_tab_content <- function(presenter) {
  markdown_text <- switch(presenter,
                          "Roy Plomley" = "
## Roy 'Roger' Plomley (1914-1985)

- Hosting period: 1942-1985

We begin, like most European countries, with an affable and curious Caucasian man.

Roy Plomley was the visionary creator of the BBC radio program 'Desert Island Discs,' which first aired in 1942. Born in 1914, Plomley was a polyvalent radio-man broadcasting, producing, and writing hundreds of times. He is rumored to have conceived the show's unique format during a late-night brainstorming session. The initial idea was simple yet profound: invite notable figures to imagine themselves cast away on a deserted island and discuss the eight records they would take with them.

40 years Plomley reigned, conducting intimate interviews that revealed the personal stories and musical tastes of his subjects. His warm demeanor and genuine curiosity set the tone for the show, and is important they are the only facets on which we can really assess the hosts. 

Plomley conducts himself with a curiousity that is playful and laid-back. His warm demeanor is almost palpable. It is clear I have a positive opinion of Roy Plomley.

Notable guests:  Paul McCartney, Elizabeth Taylor, Alfred Hitchcock, Margaret Thatcher.

    ",
                          "Michael Parkinson" = "
## Michael Parkinson (1935-2023)

- Hosting period: 1985-1986

I have been listening to DIDs for years. I have never come across a Michael Parkinson episode. May report later...?

    ",
                          "Sue Lawley" = "
## Sue 'The Law' Lawley

- Hosting period: 1986-2006

Office Sue Lawley OBE began her career in regional television before becoming a national figure as a newsreader and presenter for the BBC. Lawley's transition to 'Desert Island Discs' allowed her to delve deeper into the personal narratives of her guests, showcasing her skill as an interviewer.

Sue's warmth is hard to come by, in fact it is impossible to come by. To experience Sue's warmth her precise questioning must first be satisfied. During her 18-year tenure, Lawley conducted over 700 interviews where the guests were gently, but coldly, laid bare. Officer Lawley's style was one of lean kindness and it brought a new depth to the program allowing it to modernise and succesfully transitioning into the noughties. It appears I have a positive opinion of Sue Lawley.

Notable guests: David Attenborough, Maya Angelou, Nelson Mandela, Stephen Hawking, Tony Blair.


    ",
                          "Kirsty Young" = "
## Kirsty 'Tea' Young

- Hosting period: 2006-2018

The tea Kirsty Young offers you is extremely sweet. That is how she gets you to tell her your secrets. From Scot-land, she brought a fresh perspective to 'Desert Island Discs', during her 12 years as host she made a mark and was well-liked. Her legacy continues and just a few years ago [she hosted David Attenborough's 90th Birthday](https://www.bbc.co.uk/programmes/p03qxjzj). It seems I have a a positive opinion of Kirsty Young.

Notable guests: David Beckham, Tom Hanks, Judi Dench, Malala Yousafzai, David Cameron.

    ",
                          "Lauren Laverne" = "
## Lauren 'Lollypop' Laverne

- Hosting period: 2019-current

Lauren Laverne took over as the host of 'Desert Island Discs' in 2018. She initially gained fame as the lead singer and guitarist of the Britpop band Kenickie before transitioning to broadcasting. Her deep knowledge of music is embodied in a near-peppy on-air personality. The result is very engaging and continues to elevate the original spirit of the show, one of curiosity and warmth.

In this she joins the men and women before her who gave voice to and presentend thousands of personal stories, and considerably more music. I hold Lauren Laverne in high regard.

Notable guests: Louis Theroux, Ian Wright, Helena Bonham Carter, Paul McCartney, Nile Rodgers.


    ",
                          # Default case
                          "Content not available."
  )
  
  markdown(markdown_text)
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: black; color: white; }
      .slider-animate-button { display: none; }
      .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar { background: white; }
      .js-irs-0 .irs-line { background: #444; }
      .js-irs-0 .irs-single { background: black !important; color: white !important; }
      .js-irs-0 .irs-handle { border-color: white; background: black;}
    "))
  ),
  sliderInput("year_selector", NULL, 
              min = 1942, max = 2024, value = round(runif(1,1942,2024)),
              step = 1, width = "100%", ticks = FALSE, sep = ""),
  tabsetPanel(
    id = "tabset",
    tabPanel("Roy Plomley",value="RP", get_tab_content("Roy Plomley")),
    tabPanel("Michael Parkinson",value="MP", get_tab_content("Michael Parkinson")),
    tabPanel("Sue Lawley",value="SL", get_tab_content("Sue Lawley")),
    tabPanel("Kirsty Young",value="KY", get_tab_content("Kirsty Young")),
    tabPanel("Lauren Laverne",value="LL", get_tab_content("Lauren Laverne"))
  )
)

server <- function(input, output, session) {
   # Reactive values to track updates
  update_in_progress <- reactiveValues(slider = FALSE, tabset = FALSE)
  
  # Observer for slider input
  observeEvent(input$year_selector, {
    if (update_in_progress$slider) {
      update_in_progress$slider <- FALSE
      return()
    }
    update_in_progress$tabset <- TRUE
    updateTabsetPanel(session, "tabset", selected = 
                        if (input$year_selector <= 1985) "RP"
                      else if (input$year_selector <= 1988) "MP"
                      else if (input$year_selector <= 2006) "SL"
                      else if (input$year_selector <= 2018) "KY"
                      else "LL"
    )
  })
  
  # Observer for tabset input
  observeEvent(input$tabset, {
    if (update_in_progress$tabset) {
      update_in_progress$tabset <- FALSE
      return()
    }
    update_in_progress$slider <- TRUE
    selected_tab <- input$tabset
    new_year <- switch(selected_tab,
                       "RP" = 1942,
                       "MP" = 1986,
                       "SL" = 1989,
                       "KY" = 2007,
                       "LL" = 2019
    )
    updateSliderInput(session, "year_selector", value = new_year)
  })
}

shinyApp(ui, server)
