local awful = require("awful")

theme = {}
theme.wallpaper = awful.util.getdir("config") .. "/background.jpg"
-- }}}

background = "#000000"
foreground = "#eaeaea"
cur_line   = "#2a2a2a"
selection  = "#424242"
comment    = "#969896"
red        = "#d54e53"
orange     = "#e78c45"
yellow     = "#e7c547"
green      = "#b9ca4a"
aqua       = "#70c0b1"
blue       = "#7aa6da"
purple     = "#c397db"


-- {{{ Styles
theme.font      = "dejavu sans 9"

-- {{{ Colors
theme.fg_normal  = comment
theme.fg_focus   = blue
theme.fg_urgent  = purple
theme.bg_normal  = background
theme.bg_focus   = cur_line
theme.bg_urgent  = selection
theme.bg_systray = theme.bg_normal
-- }}}

-- {{{ Borders
theme.border_width  = 1
theme.border_normal = cur_line
theme.border_focus  = blue
theme.border_marked = purple
-- }}}

-- {{{ Titlebars
theme.titlebar_bg_focus  = blue
theme.titlebar_bg_normal = comment
-- }}}

-- {{{ Mouse finder
theme.mouse_finder_color = "#d54e53"
-- mouse_finder_[timeout|animate_timeout|radius|factor]
-- }}}

-- {{{ Menu
-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_height = 15
theme.menu_width  = 100
-- }}}

-- {{{ Icons
-- {{{ Taglist
theme.taglist_squares_sel   = "/usr/share/awesome/themes/rbown/taglist/squarefz.png"
theme.taglist_squares_unsel = "/usr/share/awesome/themes/rbown/taglist/squarez.png"
--theme.taglist_squares_resize = "false"
-- }}}

-- {{{ Misc
theme.awesome_icon           = "/usr/share/awesome/themes/rbown/awesome-icon.png"
theme.menu_submenu_icon      = "/usr/share/awesome/themes/default/submenu.png"
-- }}}

-- {{{ Layout
theme.layout_tile       = "/usr/share/awesome/themes/rbown/layouts/tile.png"
theme.layout_tileleft   = "/usr/share/awesome/themes/rbown/layouts/tileleft.png"
theme.layout_tilebottom = "/usr/share/awesome/themes/rbown/layouts/tilebottom.png"
theme.layout_tiletop    = "/usr/share/awesome/themes/rbown/layouts/tiletop.png"
theme.layout_fairv      = "/usr/share/awesome/themes/rbown/layouts/fairv.png"
theme.layout_fairh      = "/usr/share/awesome/themes/rbown/layouts/fairh.png"
theme.layout_spiral     = "/usr/share/awesome/themes/rbown/layouts/spiral.png"
theme.layout_dwindle    = "/usr/share/awesome/themes/rbown/layouts/dwindle.png"
theme.layout_max        = "/usr/share/awesome/themes/rbown/layouts/max.png"
theme.layout_fullscreen = "/usr/share/awesome/themes/rbown/layouts/fullscreen.png"
theme.layout_magnifier  = "/usr/share/awesome/themes/rbown/layouts/magnifier.png"
theme.layout_floating   = "/usr/share/awesome/themes/rbown/layouts/floating.png"
-- }}}

-- {{{ Titlebar
theme.titlebar_close_button_focus  = "/usr/share/awesome/themes/rbown/titlebar/close_focus.png"
theme.titlebar_close_button_normal = "/usr/share/awesome/themes/rbown/titlebar/close_normal.png"

theme.titlebar_ontop_button_focus_active  = "/usr/share/awesome/themes/rbown/titlebar/ontop_focus_active.png"
theme.titlebar_ontop_button_normal_active = "/usr/share/awesome/themes/rbown/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_inactive  = "/usr/share/awesome/themes/rbown/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_inactive = "/usr/share/awesome/themes/rbown/titlebar/ontop_normal_inactive.png"

theme.titlebar_sticky_button_focus_active  = "/usr/share/awesome/themes/rbown/titlebar/sticky_focus_active.png"
theme.titlebar_sticky_button_normal_active = "/usr/share/awesome/themes/rbown/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_inactive  = "/usr/share/awesome/themes/rbown/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_inactive = "/usr/share/awesome/themes/rbown/titlebar/sticky_normal_inactive.png"

theme.titlebar_floating_button_focus_active  = "/usr/share/awesome/themes/rbown/titlebar/floating_focus_active.png"
theme.titlebar_floating_button_normal_active = "/usr/share/awesome/themes/rbown/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_inactive  = "/usr/share/awesome/themes/rbown/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_inactive = "/usr/share/awesome/themes/rbown/titlebar/floating_normal_inactive.png"

theme.titlebar_maximized_button_focus_active  = "/usr/share/awesome/themes/rbown/titlebar/maximized_focus_active.png"
theme.titlebar_maximized_button_normal_active = "/usr/share/awesome/themes/rbown/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_inactive  = "/usr/share/awesome/themes/rbown/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_inactive = "/usr/share/awesome/themes/rbown/titlebar/maximized_normal_inactive.png"
-- }}}
-- }}}

return theme
