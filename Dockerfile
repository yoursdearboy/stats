FROM rocker/shiny-verse:3.6.1

RUN install2.r --error \
  cmprsk \
  ggfortify \
  kableExtra \
  rhandsontable
