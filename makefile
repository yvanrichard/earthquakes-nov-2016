all: dash

data: data/data.rdata

data/data.rdata:
	Rscript get-data.r

dash:
	Rscript -e "shiny::runApp(port=5400)"

deploy:
	Rscript -e "rsconnect::deployApp(appName='earthquakes-nov-16')"

# stage:
# 	Rscript -e "rsconnect::deployApp('dashboard', appName='freshwater-nz-staging')"

# .PHONY: test

# test:
# 	Rscript -e "shiny::runApp('test', port=5401)"

