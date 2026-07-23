# test_contrast.R
# Every text on surface pairing in the app, checked to WCAG at 4.5:1 for
# normal text across all eight theme and palette combinations. The token
# values are parsed straight out of app.css so this test reads exactly what
# the app ships, and a later edit that dims a token below threshold fails
# the build rather than reaching a user.

# ---- sRGB relative luminance and contrast ratio, per WCAG definitions ----
hex_rgb <- function(h) {
  h <- gsub("#", "", h)
  c(strtoutf <- strtoi(substr(h, 1, 2), 16L),
    strtoi(substr(h, 3, 4), 16L),
    strtoi(substr(h, 5, 6), 16L)) / 255
}
lin <- function(c) ifelse(c <= 0.03928, c / 12.92, ((c + 0.055) / 1.055) ^ 2.4)
lum <- function(rgb) { r <- lin(rgb); 0.2126 * r[1] + 0.7152 * r[2] + 0.0722 * r[3] }
ratio <- function(fg, bg) {
  a <- lum(hex_rgb(fg)); b <- lum(hex_rgb(bg))
  (max(a, b) + 0.05) / (min(a, b) + 0.05)
}

# ---- Parse the :root and each override block from the stylesheet ----
css <- readLines("www/app.css", warn = FALSE)

parse_block <- function(selector) {
  # Grab the token lines inside a given selector block.
  start <- grep(selector, css, fixed = TRUE)[1]
  if (is.na(start)) return(list())
  close <- start + which(grepl("^\\}", css[(start + 1):length(css)]))[1]
  lines <- css[start:close]
  toks <- list()
  for (ln in lines) {
    m <- regmatches(ln, regexec("--([a-z0-9-]+):\\s*(#[0-9a-fA-F]{6})", ln))[[1]]
    if (length(m) == 3) toks[[m[2]]] <- m[3]
  }
  toks
}

base_dark   <- parse_block(":root {")
light       <- parse_block("body.light-mode {")
deut        <- parse_block("body.cb-deut {")
deut_light  <- parse_block("body.light-mode.cb-deut {")
trit        <- parse_block("body.cb-trit {")
trit_light  <- parse_block("body.light-mode.cb-trit {")
mono_dark   <- parse_block("body.cb-mono {")
mono_light  <- parse_block("body.light-mode.cb-mono {")

# Resolve a full token set for one of the eight combinations by layering the
# overrides the way the cascade does.
resolve <- function(theme, palette) {
  toks <- base_dark
  # Cascade order mirrors the stylesheet: base, then theme, then palette, then
  # the compound light plus palette block which wins where it exists.
  if (theme == "light") for (k in names(light)) toks[[k]] <- light[[k]]
  if (palette == "deut") {
    for (k in names(deut)) toks[[k]] <- deut[[k]]
    if (theme == "light") for (k in names(deut_light)) toks[[k]] <- deut_light[[k]]
  }
  if (palette == "trit") {
    for (k in names(trit)) toks[[k]] <- trit[[k]]
    if (theme == "light") for (k in names(trit_light)) toks[[k]] <- trit_light[[k]]
  }
  if (palette == "mono") {
    if (theme == "light") for (k in names(mono_light)) toks[[k]] <- mono_light[[k]]
    else for (k in names(mono_dark)) toks[[k]] <- mono_dark[[k]]
  }
  toks
}

# The pairings that carry text or a meaningful mark, foreground then background.
pairs <- list(
  c("t", "bg"), c("t", "sf"), c("t", "sf2"),
  c("m", "bg"), c("m", "sf"), c("m", "sf2"),
  c("treat", "sf"), c("compare", "sf"),
  c("good", "sf"), c("warn", "sf"), c("bad", "sf"),
  c("accent", "sf")
)

combos <- expand.grid(
  theme = c("dark", "light"),
  palette = c("none", "deut", "trit", "mono"),
  stringsAsFactors = FALSE
)

THRESHOLD <- 4.5
fails <- 0; checks <- 0
for (i in seq_len(nrow(combos))) {
  toks <- resolve(combos$theme[i], combos$palette[i])
  for (p in pairs) {
    fg <- toks[[p[1]]]; bg <- toks[[p[2]]]
    if (is.null(fg) || is.null(bg)) next
    checks <- checks + 1
    r <- ratio(fg, bg)
    # The accent as a large mark and the group colors as thick strokes are
    # graphical objects, held to the 3:1 non text threshold; text tokens to 4.5.
    lim <- if (p[1] %in% c("treat", "compare", "accent")) 3.0 else THRESHOLD
    if (r < lim) {
      fails <- fails + 1
      cat(sprintf("FAIL  %s/%s  --%s on --%s  = %.2f  (need %.1f)\n",
                  combos$theme[i], combos$palette[i], p[1], p[2], r, lim))
    }
  }
}
cat(sprintf("\nContrast: %d checks across 8 combinations, %d failures\n",
            checks, fails))
if (fails > 0) quit(status = 1)
