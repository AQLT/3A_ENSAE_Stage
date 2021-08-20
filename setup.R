knitr::opts_chunk$set(echo = FALSE,
                      fig.path = "img/")
library(kableExtra)

# Fontawesome
htmltools::tagList(rmarkdown::html_dependency_font_awesome())
is_html <- knitr::is_html_output()
is_latex <- knitr::is_latex_output()
if(is_html){
    fa_arrow_circle_right <- '<i class="fas fa-arrow-circle-right"></i>'
    fa_r_project <- '<i class="fab fa-r-project"></i>'
}else {
    if(is_latex){
        fa_arrow_circle_right <- "\\faIcon{arrow-circle-right}"
        fa_r_project <- "\\faIcon{r-project}"
    }else {
        fa_arrow_circle_right <- "->"
        fa_r_project <- 'R'
    }
}

################################
########## KNITR ###############
################################
latex_emph <- function(entete = "Note", x, sep = "\n\n"){
    txt <- paste(sprintf("\\emph{%s}", x), collapse = sep)
    if(length(x)>1){
        entete <- paste0(entete,"s")
    }
    sprintf("\\emph{%s} : %s", entete, txt)
}
add_footnote_latex_plot <- function(x, options){
    if(is.null(options$fig.note) & is.null(options$fig.source)){
        return(x)
    }
    ajouts <- "\\footnotesize"
    if(!is.null(options$fig.source)){
        ajouts <- paste(ajouts,
                        latex_emph("Source", options$fig.source, sep = "---"),
                        sep = "\n\n")
    }
    if(!is.null(options$fig.note)){
        ajouts <- paste(ajouts,
                        latex_emph("Note", options$fig.note, sep = "\n\n"),
                        sep = "\n")
    }
    x <- sub("\\end{figure}",ajouts, x,fixed = TRUE)
    paste0(x,"\\end{figure}")    
}
library(htmltools)
add_footnote_html_plot <- function(x, options){
    
    if(!is.null(options$fig.source)){
        # source <- withTags(p(class = "caption", id = "source",
        #                      tagList(as.list())
        # )
        # )
        first_txt <- tags$p(options$fig.source[1], id = "source", class = "title caption")
        all_txt <- c(list(first_txt),
                       lapply(options$fig.source[-1], tags$p, class = "caption"))
        source <- withTags(
            div(class = "caption", id = "source",
                tagList(all_txt)
            )
        )
        x <- paste(x,
                   as.character(source),
                   sep = "\n")
    }
    if(!is.null(options$fig.note)){
        # note <- withTags(p(class = "caption", id = "note",
        #                    tagList(as.list(options$fig.note))
        #                    )
        #                  )
        # note <- withTags(
        #     div(class = "caption", id = "note",
        #         tagList(lapply(options$fig.note, p, class = "caption"))
        #     )
        # )
        
        first_txt <- tags$p(options$fig.note[1], id = "note", class = "title caption")
        all_txt <- c(list(first_txt),
                     lapply(options$fig.note[-1], tags$p, class = "caption"))
        note <- withTags(
            div(class = "caption", id = "note",
                tagList(all_txt)
            )
        )
        x <- paste(x,
                   as.character(note),
                   sep = "\n")
    }
    
    x
}
add_footnote <- function(x, options){
    if(is_latex){
        res <-  add_footnote_latex_plot(x, options)
    }else if(is_html){
        res <-  add_footnote_html_plot(x, options)
    }else{
        res <-  x
    }
    res
}
# local({
hook_plot <- knitr::knit_hooks$get('plot')
knitr::knit_hooks$set(plot = function(x, options) {
    if(is.null(options$fig.cap))
        return(hook_plot(x, options))
    if(is_latex){
        res <-  knitr:::hook_plot_tex(x, options)
        # res <- add_footnote_latex(res, options)
    }else if(is_html){
        res <- hook_plot(x, options)
        # res <- add_footnote_html(res, options)
    }else{
        res <-  hook_plot(x, options)
    }
    res <- add_footnote(res, options)
    res
})
# library(knitr)
# local({
#   hook_plot <- function (x, options) {
#       
#     if (options$fig.show == "animate") 
#         return(hook_plot_html(x, options))
#     base = knitr:::`%n%`(opts_knit$get("base.url"), "")
#     cap = knitr:::.img.cap(options)
#     alt = knitr:::.img.cap(options, alt = TRUE)
#     w = options[["out.width"]]
#     h = options[["out.height"]]
#     s = options$out.extra
#     a = options$fig.align
#     ai = options$fig.show == "asis"
#     lnk = options$fig.link
#     pandoc_html = cap != "" && is_html_output()
#     in_bookdown = isTRUE(opts_knit$get("bookdown.internal.label"))
#     plot1 = ai || options$fig.cur <= 1L
#     plot2 = ai || options$fig.cur == options$fig.num
#     to = pandoc_to()
#     from = pandoc_from()
#     if (is.null(w) && is.null(h) && is.null(s) && is.null(options$fig.alt) && 
#         a == "default" && !(pandoc_html && in_bookdown)) {
#         nocap = cap == "" && !is.null(to) && !grepl("^markdown", 
#             to) && (options$fig.num == 1 || ai) && !grepl("-implicit_figures", 
#             from)
#         res = sprintf("![%s](%s%s)", cap, base, .upload.url(x))
#         if (!is.null(lnk) && !is.na(lnk)) 
#             res = sprintf("[%s](%s)", res, lnk)
#         res = paste0(res, if (nocap) 
#             "<!-- -->"
#         else "", if (is_latex_output()) 
#             " "
#         else "")
#         return(res)
#     }
#     add_link = function(x) {
#         if (is.null(lnk) || is.na(lnk)) 
#             return(x)
#         sprintf("<a href=\"%s\" target=\"_blank\">%s</a>", lnk, 
#             x)
#     }
#     if (pandoc_html && !isTRUE(grepl("-implicit_figures", from))) {
#         d1 = if (plot1) 
#             sprintf("<div class=\"figure\"%s>\n", knitr:::css_text_align(a))
#         d2 = sprintf("<p class=\"caption\">Note: %s</p>", cap)
#         if(!is.null(options$comment)){
#           d2 <- sprintf("<p class='comment'>%s</p>%s",options$comment, d2)
#         }
#         img = sprintf("<img src=\"%s\" alt=\"%s\" %s />", paste0(opts_knit$get("base.url"), 
#             knitr:::.upload.url(x)), alt, knitr:::.img.attr(w, h, s))
#         img = add_link(img)
#         if (isTRUE(options$fig.topcaption)) {
#             paste0(d1, if (ai || options$fig.cur <= 1) 
#                 d2, img, if (plot2) 
#                 "</div>") 
#         }
#         else {
#             paste0(d1, img, if (plot2) 
#                 paste0("\n", d2, "\n</div>"))
#         }
#     }
#     else add_link(.img.tag(.upload.url(x), w, h, alt, c(s, sprintf("style=\"%s\"", 
#         css_align(a)))))
# }
#   knitr::knit_hooks$set(plot = function(x, options) {
#       hook_plot(x, options)
#   })
# })

library(htmltools)
carousel <- function(files, div_id, enableControl = TRUE){
    carousel_file <- function(x, active = FALSE){
        tags$div(class = sprintf("carousel-item%s", ifelse(active, " active", "")),
                 tags$img(class="d-block w-100", src = x)
        )
        
    }
    control <- NULL
    if(enableControl){
        p1 <- withTags({
            a(class ="carousel-control-prev", href = paste0("#", div_id), `data-slide`="prev",
              span(class="carousel-control-prev-icon")
            )})
        p2 <- withTags({
            a(class ="carousel-control-next", href = paste0("#", div_id), `data-slide`="next",
              span(class="carousel-control-next-icon")
            )})
        control <- tagList(p1, p2)
    }
    first_file <- carousel_file(files[[1]], active = TRUE)
    all_files <- c(list(first_file), lapply(files[-1], carousel_file, active = FALSE))
    all_files <- tags$div(class = "carousel-inner",
                          tagList(all_files)
    )
    all_files
    withTags({
        div(class="carousel slide", `data-ride`="carousel", id = div_id,
            all_files,
            control
        )
    })
}
# carousel(div_id = "carouselExampleSlidesOnly",
# files = sprintf("img/kernels/%i.png",2:4))

## Graphiques 3D

library(plot3D)
global_plot <- function(data, q, method, degree, phi = 40,
                        theta = 40,
                        titre = NULL){
    
    data_tri <- data[(data$q %in% as.numeric(q)) &
                         (data$method %in% method) &
                         (data$degree %in% as.numeric(degree)),]
    scatter_3D(data_tri, titre = titre, phi = phi, theta = theta)
}
scatter_3D <- function(x, titre = NULL, phi = 40,
                       theta = 40){
    add <- nrow(x) >0
    if(add){
        with(x, 
             scatter3D(x = fidelity.weight,
                       y = smoothness.weight,
                       z = timeliness.weight,
                       colvar = NULL, 
                       phi = phi, theta = theta,
                       # bty = "g",
                       pch = 1,
                       # cex = 0.1, alpha = 0.4,
                       ticktype = "detailed",
                       xlim = c(0,1),
                       ylim = c(0,1),
                       zlim = c(0,1), 
                       xlab = "\n\nFidelity",
                       ylab ="\n\nSmoothness",
                       zlab = "\n\nTimeliness",
                       main = titre))
        polygon3D(x = c(0,0,1), y = c(0,1,0), z = c(1,0,0),
                  add = add, alpha = 0.2,
                  ticktype = "detailed",
                  phi = phi, theta = theta,
                  xlim = c(0,1),
                  ylim = c(0,1),
                  zlim = c(0,1), 
                  xlab = "\n\nFidelity",
                  ylab ="\n\nSmoothness",
                  zlab = "\n\nTimeliness",
                  main = titre)
    }else{
        scatter3D(x = -2,
                  y = 2,
                  z = 2,
                  colvar = NULL, 
                  phi = phi, theta = theta,
                  # bty = "g",
                  pch = 1,
                  # cex = 0.1, alpha = 0.4,
                  ticktype = "detailed",
                  xlim = c(0,1),
                  ylim = c(0,1),
                  zlim = c(0,1), 
                  xlab = "\n\nFidelity",
                  ylab ="\n\nSmoothness",
                  zlab = "\n\nTimeliness",
                  main = titre)
    }
}