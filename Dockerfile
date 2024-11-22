FROM rocker/shiny:4.4.2

# Define environment variable for app directory
ENV APP_NAME=amic
ENV APP_DIR=/home/${APP_NAME}

# Make a directory in the container
RUN mkdir $APP_DIR
ADD . $APP_DIR
WORKDIR $APP_DIR

# Install pak and use it to install dependencies only
RUN R -e "install.packages('pak', repos = 'https://r-lib.github.io/p/pak/dev/')" && \
    R -e "pak::local_install(upgrade=FALSE, dependencies = TRUE)" 

# Expose the application port
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');library(${APP_NAME});${APP_NAME}::run_app()"
