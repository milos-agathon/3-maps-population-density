###################################################
# 3 easy ways to map population density
# from gridded raster
# Milos Popovic 2023/09/12
###################################################

libs <- c(
    "tidyverse", "terra",
    "giscoR", "sf", "ggmap",
    "rayshader"
)

installed_libs <- libs %in% rownames(
    installed.packages()
)

if(any(installed_libs == F)){
    install.packages(
        libs[!installed_libs]
    )
}

invisible(lapply(
    libs, library, character.only =T
))

# 1. GET DATA
#-------------

URL <- "https://data.worldpop.org/GIS/Population/Global_2021_2022_1km_UNadj/unconstrained/2022/NLD/nld_ppp_2022_1km_UNadj.tif"

population_data_1km <- terra::rast(URL)

terra::plot(population_data_1km)

# 2. RASTER TO DF
#----------------

population_df <- population_data_1km |>
    as.data.frame(xy = T)

head(population_df)
names(population_df)[3] <- "pop"

# 3. THEME & COLORS
#------------------

theme_for_the_win <- function() {
    theme_minimal() +
    theme(
        axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "top",
        legend.title = element_text(
            size = 9, color = "grey20",
            vjust = -1, hjust = .5
        ),
        legend.text = element_text(
            size = 8, color = "grey20"
        ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(
            fill = "white", color = NA
        ),
        plot.margin = unit(
            c(
                t = 0, r = 0,
                b = 0, l = 0
            ), "lines"
        )
    )
}

cols <- hcl.colors(5, "Viridis")

pal <- colorRampPalette(cols)(512)

# 4. BACKGROUND
#--------------

main_dir <- getwd()
country_sf <- giscoR::gisco_get_countries(
    country = "NL",
    resolution = "3"
)

country_bbox <- sf::st_bbox(
    country_sf
)

country_coords <- c(
    country_bbox[["xmin"]],
    country_bbox[["ymin"]],
    country_bbox[["xmax"]],
    country_bbox[["ymax"]]
)

country_layer <- ggmap::get_stamenmap(
    country_coords,
    zoom = 8,
    maptype = "terrain",
    color = "bw",
    force = T
)

ggmap(country_layer)

# METHOD I: CHOROPLETH MAP
#----------------------------

p1 <- ggplot() +
    geom_raster(
        data = population_df,
        aes(
            x = x,
            y = y,
            fill = pop
        )
    ) +
    scale_fill_gradientn(
        name = "People per 1"~km^2,
        colors = pal,
        na.value = NA
    ) +
    guides(
        fill = guide_colorbar(
            direction = "horizontal",
            barwidth = 12,
            barheight = .25,
            title.position = "top"
        )
    ) +
    theme_for_the_win()

# METHOD II: SPIKES
#-------------------

w <- ncol(population_data_1km)
h <- nrow(population_data_1km)

rayshader::plot_gg(
    ggobj = p1,
    width = w / 50,
    height = h / 50,
    windowsize = c(w, h),
    scale = 750,
    solid = F,
    shadow = T,
    shadow_intensity = 1,
    sunangle = 225,
    phi = 85,
    theta = 0,
    zoom = .55
)

rayshader::render_highquality(
    filename = "population2.png",
    preview = T,
    interactive = F,
    light = T,
    lightdirection = 225,
    lightintensity = 1250,
    lightaltitude = 90,
    width = w * 10,
    height = h * 10
)

# METHOD III: CONTOUR
#--------------------

p3 <- ggmap::ggmap(country_layer) +
    geom_contour(
        data = population_df,
        aes(
            x = x,
            y = y,
            z = pop,
            colour = after_stat(level)
        ), linewidth = .2,
        inherit.aes = F
    ) +
    scale_color_gradientn(
        name = "People per 1"~km^2,
        colors = pal
    ) +
    guides(
        color = guide_colorbar(
            direction = "horizontal",
            barwidth = 12,
            barheight = .25
        )
    ) +
    coord_cartesian() +
    theme_for_the_win()
