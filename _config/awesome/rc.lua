-- Awesome configuration file, using awesome 3.2 on Arch GNU/Linux.
--   * Adrian C. <anrxc_at_sysphere_org>

-- Updated on: Mar 6, 02:29:49 CET 2009
-- Screenshot: http://sysphere.org/gallery/snapshots/awesome

-- FAQ:
--   1. Statusbar widgets made with Wicked:
--        - http://git.glacicle.com/?p=awesome/wicked.git

--   2. Widget icons are from dzen:
--        - http://dzen.geekmode.org/wiki/wiki.cgi/-main/DzenIconPacks

--      2a. All my icons can be found here (always someone asking for it):
--            - http://sysphere.org/~anrxc/icons-anrxc-14px.tar.gz

--   3. Why these colors?
--        It's Zenburn. My Emacs, terminal emulator, mail client... all use these colors.
--          - http://slinky.imukuppi.org/zenburnpage/

--      3a. My Zenburn theme file can be found here:
--            - http://sysphere.org/~anrxc/local/scr/dotfiles/awesome-zenburn.html

--      3b. My .Xdefaults can be found here:
--            - http://sysphere.org/~anrxc/local/scr/dotfiles/Xdefaults

--   4. Fonts used on my desktop:
--        Profont   : http://www.tobias-jung.de/seekingprofont
--        Terminus  : http://www.is-vn.bg/hamster
--        Liberation: http://www.redhat.com/promo/fonts/

--   5. My .xinitrc can be found here:
--        - http://sysphere.org/~anrxc/local/scr/dotfiles/xinitrc-awesome

-- This work is licensed under the Creative Commons Attribution License.
-- To view a copy of this license, visit http://creativecommons.org/licenses/by/3.0/


-- Load libraries
require("awful")
require("beautiful")
require("wicked")
require("naughty")
require("mailcheck")

-- {{{ Variable definitions
--
-- User styles for windows, statusbars, titlebars and widgets
theme_path = os.getenv("HOME") .. "/.config/awesome/themes/zenburn"
beautiful.init(theme_path)
--
-- Modifier keys
modkey = "Mod4" -- Super_L

terminal = "urxvtcd"
editor = "emacsclient -c"
--
-- Window titlebars
use_titlebar = false
--
-- Window management layouts
layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
    awful.layout.suit.magnifier,
    awful.layout.suit.floating
}
--
-- Application specific behaviour
apprules = {
    -- Class       Instance       Name                Screen          Tag   Floating
    {  nil,        nil,           "mocp",             nil,            nil,  true  },
    {  nil,        nil,           "htop",             nil,            nil,  true  },
    { "Firefox",   nil,           nil,                screen.count(),   2,  false },
    { "Firefox",   "Download",    nil,                nil,            nil,  true  },
    { "Firefox",   "Places",      nil,                nil,            nil,  true  },
    { "Firefox",   "Greasemonkey",nil,                nil,            nil,  true  },
    { "Firefox",   "Extension",   nil,                nil,            nil,  true  },
    { "MPlayer",   nil,           nil,                nil,            nil,  true  },
    {  nil,        nil,           "VLC media player", nil,            nil,  true  },
    { "Gimp",      nil,           nil,                nil,            nil,  true  },
    { "Xmessage",  "xmessage",    nil,                nil,            nil,  true  },
    { "Kcalc",     "kcalc",       nil,                nil,            nil,  true  },
}
-- }}}


-- {{{ Tags
--
-- Define tags table
tags = {}
tags.settings = {
    { name = "α",  layout = layouts[1], setslave = true },
    { name = "β", layout = layouts[1], setslave = true },
    { name = "γ",   layout = layouts[1]  },
    { name = "δ",  layout = layouts[4]  },
    { name = "ε",    layout = layouts[3]  },
    { name = "ζ",     layout = layouts[10] },
    { name = "η",     layout = layouts[10] },
    { name = "θ",   layout = layouts[3]  },
    { name = "ι", layout = layouts[10] }
}
-- Initialize tags
for s = 1, screen.count() do
    tags[s] = {}
    for i, v in ipairs(tags.settings) do
        tags[s][i] = tag(v.name)
        tags[s][i].screen = s
        awful.tag.setproperty(tags[s][i], "layout",   v.layout)
        awful.tag.setproperty(tags[s][i], "setslave", v.setslave)
        awful.tag.setproperty(tags[s][i], "mwfact",   v.mwfact)
        awful.tag.setproperty(tags[s][i], "nmaster",  v.nmaster)
        awful.tag.setproperty(tags[s][i], "ncols",    v.ncols)
        awful.tag.setproperty(tags[s][i], "icon",     v.icon)
    end
    tags[s][1].selected = true
end
-- }}}


-- {{{ Wibox
--
-- Widgets configuration
--
-- Reusable separators
myspace          = widget({ type = "textbox", name = "myspace", align = "right" })
myseparator      = widget({ type = "textbox", name = "myseparator", align = "right" })
myspace.text     = " "
myseparator.text = "|"
--
-- CPU usage graph and temperature
mycpuicon        = widget({ type = "imagebox", name = "mycpuicon", align = "right" })
mycpuicon.image  = image(beautiful.widget_cpu)
mycputempwidget  = widget({ type = "textbox", name = "mycputempwidget", align = "right" })
mycpugraphwidget = widget({ type = "graph", name = "mycpugraphwidget", align = "right" })
mycpugraphwidget.width        = 70
mycpugraphwidget.height       = 0.90
mycpugraphwidget.grow         = "left"
mycpugraphwidget.bg           = beautiful.fg_off_widget
mycpugraphwidget.border_color = beautiful.border_widget
mycpugraphwidget:plot_properties_set("cpu", {
    fg        = beautiful.fg_widget,
    fg_center = beautiful.fg_center_widget,
    fg_end    = beautiful.fg_end_widget,
    vertical_gradient = false
})

function get_temp()
    local filedescriptor = io.popen('awk \'{print $2 "°C"}\' /proc/acpi/thermal_zone/TZ00/temperature')
    if not filedescriptor then
       return {"-"}
    end
    local value = filedescriptor:read()
    filedescriptor:close()
    return {value}
end
wicked.register(mycputempwidget, get_temp, "$1", 20)
wicked.register(mycpugraphwidget, wicked.widgets.cpu, "$1", 2, "cpu")
mycpugraphwidget:buttons({button({ }, 1, function () awful.util.spawn(terminal .. " -name htop -e htop --sort-key PERCENT_CPU") end)})

--
-- Battery percentage and state indicator
mybaticon       = widget({ type = "imagebox", name = "mybaticon", align = "right" })
mybaticon.image = image(beautiful.widget_bat)
mybatwidget     = widget({ type = "textbox", name = "mybatwidget", align = "right" })
function get_batstate()
    local filedescriptor = io.popen('acpitool -b | awk \'{sub(\/discharging,\/,"-")sub(\/charging,|charged,\/,"+")sub(\/\\.\/," "); print $4 substr($5,1,3)"%%"}\'')
    local value = filedescriptor:read()
    filedescriptor:close()
    return {value}
end
wicked.register(mybatwidget, get_batstate, "$1", 20)

--
-- Memory usage bar
mymemicon       = widget({ type = "imagebox", name = "mymemicon", align = "right" })
mymemicon.image = image(beautiful.widget_mem)
mymembarwidget  = widget({ type = "progressbar", name = "mymembarwidget", align = "right" })
mymembarwidget.width          = 10
mymembarwidget.height         = 0.9
mymembarwidget.gap            = 0
mymembarwidget.border_padding = 1
mymembarwidget.border_width   = 0
mymembarwidget.ticks_count    = 4
mymembarwidget.ticks_gap      = 1
mymembarwidget.vertical       = true
mymembarwidget:bar_properties_set("mem", {
    bg        = beautiful.bg_widget,
    fg        = beautiful.fg_widget,
    fg_center = beautiful.fg_center_widget,
    fg_end    = beautiful.fg_end_widget,
    fg_off    = beautiful.fg_off_widget,
    min_value = 0,
    max_value = 100
})
wicked.register(mymembarwidget, wicked.widgets.mem, "$1", 20, "mem")
mymembarwidget:buttons({button({ }, 1, function () awful.util.spawn(terminal .. " -name htop -e htop --sort-key PERCENT_MEM") end)})

--
-- File system usage bars
-- myfsicon       = widget({ type = "imagebox", name = "myfsicon", align = "right" })
-- myfsicon.image = image(beautiful.widget_fs)
-- myfsbarwidget  = widget({ type = "progressbar", name = "myfsbarwidget", align = "right" })
-- myfsbarwidget.width          = 20
-- myfsbarwidget.height         = 0.9
-- myfsbarwidget.gap            = 1
-- myfsbarwidget.border_padding = 1
-- myfsbarwidget.border_width   = 0
-- myfsbarwidget.ticks_count    = 4
-- myfsbarwidget.ticks_gap      = 1
-- myfsbarwidget.vertical       = true
-- myfsbarwidget:bar_properties_set("rootfs", {
--     bg        = beautiful.bg_widget,
--     fg        = beautiful.fg_widget,
--     fg_center = beautiful.fg_center_widget,
--     fg_end    = beautiful.fg_end_widget,
--     fg_off    = beautiful.fg_off_widget,
--     min_value = 0,
--     max_value = 100
-- })
-- myfsbarwidget:bar_properties_set("homefs", {
--     bg        = beautiful.bg_widget,
--     fg        = beautiful.fg_widget,
--     fg_center = beautiful.fg_center_widget,
--     fg_end    = beautiful.fg_end_widget,
--     fg_off    = beautiful.fg_off_widget,
--     min_value = 0,
--     max_value = 100
-- })
-- myfsbarwidget:bar_properties_set("storagefs", {
--     bg        = beautiful.bg_widget,
--     fg        = beautiful.fg_widget,
--     fg_center = beautiful.fg_center_widget,
--     fg_end    = beautiful.fg_end_widget,
--     fg_off    = beautiful.fg_off_widget,
--     min_value = 0,
--     max_value = 100
-- })
-- myfsbarwidget:bar_properties_set("backupfs", {
--     bg        = beautiful.bg_widget,
--     fg        = beautiful.fg_widget,
--     fg_center = beautiful.fg_center_widget,
--     fg_end    = beautiful.fg_end_widget,
--     fg_off    = beautiful.fg_off_widget,
--     min_value = 0,
--     max_value = 100
-- })
-- wicked.register(myfsbarwidget, wicked.widgets.fs, "${/ usep}", 240, "rootfs")
-- wicked.register(myfsbarwidget, wicked.widgets.fs, "${/home usep}", 240, "homefs")
-- wicked.register(myfsbarwidget, wicked.widgets.fs, "${/data usep}", 240, "storagefs")
-- wicked.register(myfsbarwidget, wicked.widgets.fs, "${/backup usep}", 240, "backupfs")
-- myfsbarwidget:buttons({button({ }, 1, function () awful.util.spawn("rox") end)})

--
-- Network usage statistics
myneticon         = widget({ type = "imagebox", name = "myneticon", align = "right" })
myneticonup       = widget({ type = "imagebox", name = "myneticonup", align = "right" })
myneticon.image   = image(beautiful.widget_net)
myneticonup.image = image(beautiful.widget_netup)
mynetwidget       = widget({ type = "textbox", name = "mynetwidget", align = "right" })
mywifiwidget      = widget({ type = "textbox", name = "mywifiwidget", align = "right" })
wicked.register(mynetwidget, wicked.widgets.net,
    '<span color="'..beautiful.fg_netdn_widget..'">${eth0 down_kb}</span> <span color="'..beautiful.fg_netup_widget..'">${eth0 up_kb}</span>', 2)
wicked.register(mywifiwidget, wicked.widgets.net,
    '<span color="'..beautiful.fg_netdn_widget..'">${wlan0 down_kb}</span> <span color="'..beautiful.fg_netup_widget..'">${wlan0 up_kb}</span>', 2)

--
-- Mail subject (latest e-mail)
mymailicon       = widget({ type = "imagebox", name = "mymailicon", align = "right" })
mymailicon.image = image(beautiful.widget_mail)
mymailwidget     = widget({ type = "textbox", align = "right", name = "mail", width = 100 })

mailcheck.register( mymailwidget, os.getenv("HOME") .. "/.maildir", beautiful.bg_widget, beautiful.fg_widget )
wicked.register(mymailwidget, mailcheck.check, " mail: $1 ")

--
-- Volume level progressbar and changer
-- myvolicon       = widget({ type = "imagebox", name = "myvolicon", align = "right" })
-- myvolicon.image = image(beautiful.widget_vol)
-- myvolwidget     = widget({ type = "textbox", name = "myvolwidget", align = "right" })
-- myvolbarwidget  = widget({ type = "progressbar", name = "myvolbarwidget", align = "right" })
-- myvolbarwidget.width          = 10
-- myvolbarwidget.height         = 0.9
-- myvolbarwidget.gap            = 0
-- myvolbarwidget.border_padding = 1
-- myvolbarwidget.border_width   = 0
-- myvolbarwidget.ticks_count    = 4
-- myvolbarwidget.ticks_gap      = 1
-- myvolbarwidget.vertical       = true
-- myvolbarwidget:bar_properties_set("volume", {
--     bg        = beautiful.bg_widget,
--     fg        = beautiful.fg_widget,
--     fg_center = beautiful.fg_widget,
--     fg_end    = beautiful.fg_end_widget,
--     fg_off    = beautiful.fg_off_widget,
--     min_value = 0,
--     max_value = 100
-- })
-- function get_volstate()
--     local filedescriptor = io.popen('amixer get PCM | awk \'{ field = $NF }; END{sub(/\%/," "); print substr($5,2,3)}\'')
--     local value = filedescriptor:read()
--     filedescriptor:close()
--     return {value}
-- end
-- wicked.register(myvolwidget, get_volstate, "$1%", 2)
-- wicked.register(myvolbarwidget, get_volstate, "$1", 2, "volume")
-- myvolbarwidget:buttons({
--     button({ }, 1, function () awful.util.spawn("kmix") end),
--     button({ }, 2, function () awful.util.spawn("amixer -q sset Master toggle") end),
--     button({ }, 4, function () awful.util.spawn("amixer -q sset PCM 2dB+") end),
--     button({ }, 5, function () awful.util.spawn("amixer -q sset PCM 2dB-") end)
-- })
-- myvolwidget:buttons(myvolbarwidget:buttons())

--
-- Date, time and...
-- the current agenda popup
org_agenda_pupup = nil

-- do some highlighting and show the popup
function show_org_agenda ()
   local fd = io.open("/tmp/org-agenda.txt", "r")
   if not fd then
      return
   end
   local text = fd:read("*a")
   fd:close()
   -- highlight week agenda line
   text = text:gsub("(Week%-agenda[ ]+%([^ ]+%):)", "<span color='#84CAF5'><u>%1</u></span>")
   -- highlight dates
   text = text:gsub("(%w+[ ]+%d%d? %w+ %d%d%d%d[^\n]*)", "<span color='#FDA401'>%1</span>")
   -- highlight times
   text = text:gsub("(%d%d?:%d%d)", "<span color='#E6D780'>%1</span>")
   -- highlight tags
   text = text:gsub("(:[^ ]+:)([ ]*\n)", "<span color='#F5DFB4'>%1</span>%2")
   -- highlight TODOs
   text = text:gsub("(TODO) ", "<span color='#FF0000'><b>%1</b></span> ")
   -- highlight categories
   text = text:gsub("([ ]+%w+:) ", "<span color='#FF7F24'>%1</span> ")
   org_agenda_pupup = naughty.notify(
      { text     = text,
        font     = "Monospace 8",
        icon     = "/usr/share/icons/oxygen/64x64/apps/kontact.png",
        timeout  = 999999999,
        width    = 600,
        position = "top_right",
        screen   = mouse.screen })
end

-- dispose the popup
function dispose_org_agenda ()
   if org_agenda_pupup ~= nil then
      naughty.destroy(org_agenda_pupup)
      org_agenda_pupup = nil
   end
end

mydateicon       = widget({ type = "imagebox", name = "mydateicon", align = "right" })
mydateicon.image = image(beautiful.widget_date)
mydatewidget     = widget({ type = "textbox", name = "mydatewidget", align = "right" })
wicked.register(mydatewidget, wicked.widgets.date, "%b %e, %R", 60)

mydatewidget.mouse_enter = show_org_agenda
mydatewidget.mouse_leave = dispose_org_agenda


-- -- a Calendar
-- function calendar_select(offset)
--     local datespec = os.date("*t")
--     datespec = datespec.year * 12 + datespec.month - 1 + offset
-- --  awful.util.spawn("cal -m " .. (datespec % 12 + 1) .. " " .. math.floor(datespec / 12) .. " | xmessage -geometry +1135+17 -file -")
--     awful.util.spawn("~/code/python/devel/projects/pylendar.py " .. (datespec % 12 + 1))
-- end
-- mydatewidget:buttons({
--     button({ }, 1, function () calendar_select(0) end),
--     button({ }, 4, function () calendar_select(1) end),
--     button({ }, 5, function () calendar_select(-1) end)
-- })
--
-- System tray
mysystray = widget({ type = "systray", align = "right" })
--
--
-- Create a wibox and...
mywibox     = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist   = {}
mytaglist.buttons = { button({ }, 1, awful.tag.viewonly),
                      button({ modkey }, 1, awful.client.movetotag),
                      button({ }, 3, function (tag) tag.selected = not tag.selected end),
                      button({ modkey }, 3, awful.client.toggletag),
                      button({ }, 4, awful.tag.viewnext),
                      button({ }, 5, awful.tag.viewprev) }
mytasklist = {}
mytasklist.buttons = { button({ }, 1, function (c)
                                          if not c:isvisible() then
                                              awful.tag.viewonly(c:tags()[1])
                                          end
                                          client.focus = c
                                          c:raise()
                                      end),
                       button({ }, 3, function () if instance then instance:hide() end instance = awful.menu.clients({ width=250 }) end),
                       button({ }, 4, function ()
                                          awful.client.focus.byidx(1)
                                          if client.focus then client.focus:raise() end
                                      end),
                       button({ }, 5, function ()
                                          awful.client.focus.byidx(-1)
                                          if client.focus then client.focus:raise() end
                                      end) }

-- ...add it to each screen
for s = 1, screen.count() do
    -- Create a promptbox
    mypromptbox[s] = widget({ type = "textbox", align = "left" })
    -- Create an imagebox widget with icons indicating active layout
    mylayoutbox[s] = widget({ type = "imagebox", align = "left" })
    mylayoutbox[s]:buttons({ button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                             button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                             button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                             button({ }, 5, function () awful.layout.inc(layouts, -1) end) })
    -- Create the taglist
    mytaglist[s] = awful.widget.taglist.new(s, awful.widget.taglist.label.all, mytaglist.buttons)
    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist.new(function(c)
                                                  return awful.widget.tasklist.label.currenttags(c, s)
                                              end, mytasklist.buttons)
    -- Create the wibox
    mywibox[s] = wibox({ position = "top", height = "14", fg = beautiful.fg_normal, bg = beautiful.bg_normal })
    -- Add widgets to the wibox (order matters)
    mywibox[s].widgets = { mytaglist[s],
                           mytasklist[s],
                           mylayoutbox[s],
                           mypromptbox[s],
                           myneticon, mynetwidget, myneticonup,
                           myseparator,
                           -- myneticon, mywifiwidget, myneticonup,
                           -- myseparator,
                           mycpuicon, myspace,
                           mycputempwidget, myspace, mycpugraphwidget,
                           myseparator,
                           mybaticon, mybatwidget, myspace,
                           myseparator,
                           mymemicon, myspace, mymembarwidget, myspace,
                           myseparator,
                           -- myfsicon, myfsbarwidget, myspace,
                           -- myseparator,
                           mymailicon, myspace, mymailwidget, myspace,
                           myseparator,
                           -- myvolicon, myvolwidget, myspace, myvolbarwidget, myspace,
                           -- myseparator,
                           mydateicon, mydatewidget,
                           myseparator,
                           s == screen.count() and mysystray or nil
    }
    mywibox[s].screen = s
end
-- }}}


-- {{{ Mouse bindings
root.buttons({
    button({ }, 3, function () awful.prompt.run({ prompt = "Run: " }, mypromptbox[mouse.screen],
                       awful.util.spawn, awful.completion.bash, awful.util.getdir("cache") .. "/history")
                   end),
    button({ }, 4, awful.tag.viewnext),
    button({ }, 5, awful.tag.viewprev)
})
-- }}}


-- {{{ Key bindings
globalkeys = {
    -- Applications
    key({ modkey }, "Return",function () awful.util.spawn(terminal) end),
    key({ modkey }, "g", function () awful.util.spawn(editor .. " --eval '(gnus)'") end),
    key({ modkey }, "e", function () awful.util.spawn("emacs-cvs") end),
    key({ modkey }, "BackSpace", function () awful.util.spawn(terminal .. " -name mocp -e mocp") end),
    key({ modkey }, "w", function () awful.util.spawn("firefox") end),
    --
    -- Prompt menus
    key({ modkey }, "F1", function () awful.prompt.run({ prompt = "Run: " }, mypromptbox[mouse.screen],
                              awful.util.spawn, awful.completion.bash, awful.util.getdir("cache") .. "/history")
                          end),
    key({ modkey }, "F2", function () awful.prompt.run({ prompt = "Manual: " }, mypromptbox[mouse.screen],
                              function (page) awful.util.spawn(terminal .. " -fg '" .. beautiful.fg_focus .. "' -e man " .. page) end,
                                  function(cmd, cur_pos, ncomp)
                                      local c, err = io.popen("for i in /usr/share/man/man?;do ls $i; done | cut -d. -f1")
                                      local pages = {}
                                      if c then while true do
                                              local manpage = c:read("*line")
                                              if not manpage then break end
                                              if manpage:find("^" .. cmd:sub(1, cur_pos)) then table.insert(pages, manpage) end
                                          end
                                          c:close()
                                      else io.stderr:write(err) end
                                      if #cmd == 0 then return cmd, cur_pos end
                                      if #pages == 0 then return end
                                      while ncomp > #pages do ncomp = ncomp - #pages end
                                      return pages[ncomp], cur_pos
                                  end)
                          end),
    key({ modkey }, "F3",function () awful.prompt.run({ prompt = "Connect: " }, mypromptbox[mouse.screen],
                              function (host) awful.util.spawn(terminal .. " -e ssh " .. host) end)
                          end),
    key({ modkey }, "F4", function () awful.prompt.run({ prompt = "Run Lua code: " }, mypromptbox[mouse.screen],
                              awful.util.eval, awful.prompt.bash, awful.util.getdir("cache") .. "/history_eval")
                          end),

    key({ modkey }, "F12", function () awful.spawn('xlock') end),

    --
    -- Awesome controls
    key({ modkey, "Shift" }, "q", awesome.quit),
    key({ modkey, "Shift" }, "r", function () mypromptbox[mouse.screen].text = awful.util.escape(awful.util.restart()) end),
    --
    -- Tag browsing
    key({ modkey }, "n",      awful.tag.viewnext),
    key({ modkey }, "p",      awful.tag.viewprev),
    key({ modkey }, "Escape", awful.tag.history.restore),
    --
    -- Layout manipulation
    key({ modkey }, "l",          function () awful.tag.incmwfact(0.05) end),
    key({ modkey }, "h",          function () awful.tag.incmwfact(-0.05) end),
    key({ modkey, "Shift" }, "l", function () awful.client.incwfact(-0.05) end),
    key({ modkey, "Shift" }, "h", function () awful.client.incwfact(0.05) end),
    key({ modkey }, "space",          function () awful.layout.inc(layouts, 1) end),
    key({ modkey, "Shift" }, "space", function () awful.layout.inc(layouts, -1) end),
    key({ modkey, "Shift" }, "l",     function () awful.tag.incnmaster(-1) end),
    key({ modkey, "Shift" }, "h",     function () awful.tag.incnmaster(1) end),
    key({ modkey, "Control" }, "l",   function () awful.tag.incncol(-1) end),
    key({ modkey, "Control" }, "h",   function () awful.tag.incncol(1) end),
    --
    -- Focus controls
    key({ modkey }, "Tab", function () awful.client.focus.history.previous(); if client.focus then client.focus:raise() end end),
    key({ modkey }, "j",   function () awful.client.focus.byidx(1);   if client.focus then client.focus:raise() end end),
    key({ modkey }, "k",   function () awful.client.focus.byidx(-1);  if client.focus then client.focus:raise() end end),
    key({ modkey }, "#48", function () awful.client.focus.bydirection("down"); if client.focus then client.focus:raise() end end),
    key({ modkey }, "#34", function () awful.client.focus.bydirection("up");   if client.focus then client.focus:raise() end end),
    key({ modkey }, "#47", function () awful.client.focus.bydirection("left"); if client.focus then client.focus:raise() end end),
    key({ modkey }, "#51", function () awful.client.focus.bydirection("right");if client.focus then client.focus:raise() end end),
    key({ modkey, "Shift" }, "j",   function () awful.client.swap.byidx(1) end),
    key({ modkey, "Shift" }, "k",   function () awful.client.swap.byidx(-1) end),
    key({ modkey, "Shift" }, "#48", function () awful.client.swap.bydirection("down") end),
    key({ modkey, "Shift" }, "#34", function () awful.client.swap.bydirection("up") end),
    key({ modkey, "Shift" }, "#47", function () awful.client.swap.bydirection("left") end),
    key({ modkey, "Shift" }, "#51", function () awful.client.swap.bydirection("right") end),
    key({ modkey, "Control" }, "j", function () awful.screen.focus(1) end),
    key({ modkey, "Control" }, "k", function () awful.screen.focus(-1) end),
    -- With 'hide' (and optional 'sticky') bindings below, this provides a scratchpad replacement
    key({ modkey }, "s", function () for k, c in pairs(awful.client.getmarked()) do
                                 awful.client.movetotag(awful.tag.selected(mouse.screen), c)
                                 awful.client.togglemarked(c)
                                 awful.placement.centered(c)
                                 c.hide = false
                                 client.focus = c
                                 c:raise()
                             end
                         end),
    key({ modkey }, "Tab", awful.client.urgent.jumpto),
}
--
-- Client manipulation
clientkeys = {
    key({ modkey }, "b", function () if mywibox[mouse.screen].screen == nil then mywibox[mouse.screen].screen = mouse.screen else mywibox[mouse.screen].screen = nil end end),
    key({ modkey }, "c", function (c) c:kill() end),
    key({ modkey }, "f", function (c) c.fullscreen = not c.fullscreen end),
    key({ modkey }, "m", function (c) c.maximized_horizontal = not c.maximized_horizontal c.maximized_vertical = not c.maximized_vertical end),
    key({ modkey }, "o",     function () awful.client.moveresize(20, 20, -20, -20) end),
    key({ modkey }, "p",     function () awful.client.moveresize(-20, -20, 20, 20) end),
    key({ modkey }, "Down",  function () awful.client.moveresize(0, 20, 0, 0) end),
    key({ modkey }, "Up",    function () awful.client.moveresize(0, -20, 0, 0) end),
    key({ modkey }, "Left",  function () awful.client.moveresize(-20, 0, 0, 0) end),
    key({ modkey }, "Right", function () awful.client.moveresize(20, 0, 0, 0) end),
    key({ modkey, "Shift" }, "0",   function (c) c.sticky = not c.sticky end),
    key({ modkey, "Shift" }, "c",   function (c) awful.util.spawn("kill -CONT " .. c.pid) end),
    key({ modkey, "Shift" }, "s",   function (c) awful.util.spawn("kill -STOP " .. c.pid) end),
    key({ modkey, "Shift" }, "t",   function (c) if c.titlebar then awful.titlebar.remove(c) else awful.titlebar.add(c, { modkey = modkey }) end end),
    key({ modkey, "Control" }, "m", awful.client.movetoscreen),
    key({ modkey, "Control" }, "r", function (c) c:redraw() end),
    key({ modkey, "Control" }, "space",  awful.client.floating.toggle),
    key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    --
    -- Client tagging
    key({ modkey }, "t", awful.client.togglemarked),
    key({ modkey }, "d", function (c) if awful.client.ismarked(c) then c.hide = not c.hide else awful.client.togglemarked(c) c.hide = not c.hide end end),
}
--
-- Bind keyboard digits
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end
--
-- Tag controls
for i = 1, keynumber do
    table.insert(globalkeys,
        key({ modkey }, i,
            function ()
                local screen = mouse.screen
                if tags[screen][i] then
                    awful.tag.viewonly(tags[screen][i])
                end
            end))
    table.insert(globalkeys,
    key({ modkey, "Control" }, i,
            function ()
                local screen = mouse.screen
                if tags[screen][i] then
                    tags[screen][i].selected = not tags[screen][i].selected
                end
            end))
    table.insert(globalkeys,
    key({ modkey, "Shift" }, i,
            function ()
                if client.focus and tags[client.focus.screen][i] then
                    awful.client.movetotag(tags[client.focus.screen][i])
                end
            end))
    table.insert(globalkeys,
    key({ modkey, "Shift", "Control" }, i,
            function ()
                if client.focus and tags[client.focus.screen][i] then
                    awful.client.toggletag(tags[client.focus.screen][i])
                end
            end))
end
--
-- Mod+Shift+Fn moves marked client(s) to tag 'n'
for i = 1, keynumber do
    table.insert(globalkeys, key({ modkey, "Shift" }, "F" .. i,
                 function ()
                     local screen = mouse.screen
                     if tags[screen][i] then
                         for k, c in pairs(awful.client.getmarked()) do
                             awful.client.movetotag(tags[screen][i], c)
                         end
                     end
                 end))
end
--
-- Set keys
root.keys(globalkeys)
-- }}}



-- {{{ Hooks
--
-- Hook function to execute when focusing a client
awful.hooks.focus.register(function (c)
    if not awful.client.ismarked(c) then
        c.border_color = beautiful.border_focus
    end
end)
--
-- Hook function to execute when unfocusing a client
awful.hooks.unfocus.register(function (c)
    if not awful.client.ismarked(c) then
        c.border_color = beautiful.border_normal
    end
end)
--
-- Hook function to execute when marking a client
awful.hooks.marked.register(function (c)
    c.border_color = beautiful.border_marked
end)
--
-- Hook function to execute when unmarking a client
awful.hooks.unmarked.register(function (c)
    c.border_color = beautiful.border_focus
end)
--
-- Hook function to execute when the mouse enters a client
awful.hooks.mouse_enter.register(function (c)
    -- Sloppy focus (but disabled for magnifier layout)
    if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
        and awful.client.focus.filter(c) then
            client.focus = c
    end
end)
--
-- Hook function to execute when a new client appears
awful.hooks.manage.register(function (c)
    -- If we are not managing this application at startup, move it to the screen where the mouse is
    if not startup and awful.client.focus.filter(c) then
        c.screen = mouse.screen
    end

    --
    -- Add a titlebar to each client
    if use_titlebar then
        awful.titlebar.add(c, { modkey = modkey })
    end
    --
    -- Set client mouse bindings
    c:buttons({
        button({ }, 1, function (c) client.focus = c; c:raise() end),
        button({ modkey }, 1, awful.mouse.client.move),
        button({ modkey }, 3, awful.mouse.client.resize)
    })
    --
    -- New clients may not receive focus if they're not focusable, so set the border anyway
    c.border_width = beautiful.border_width
    c.border_color = beautiful.border_normal
    --
    -- Check application->screen/tag mappings and floating state
    local target_screen
    local target_tag
    local target_float
    for index, rule in pairs(apprules) do
        if  (((rule[1] == nil) or (c.class    and c.class    == rule[1]))
        and  ((rule[2] == nil) or (c.instance and c.instance == rule[2]))
        and  ((rule[3] == nil) or (c.name     and string.find(c.name, rule[3], 1, true)))) then
            target_screen = rule[4]
            target_tag    = rule[5]
            target_float  = rule[6]
        end
    end


    -- Apply mappings, if any
    if target_float  then
        awful.client.floating.set(c, target_float)
    end
    if target_screen then
        c.screen = target_screen
        awful.client.movetotag(tags[target_screen][target_tag], c)
    end

    client.focus = c

    --
    -- Set client key bindings
    c:keys(clientkeys)
    --
    -- Put windows at the end of others instead of setting them as a master
    --awful.client.setslave(c)
    -- ...or do it selectively for certain tags
    if awful.tag.getproperty(awful.tag.selected(mouse.screen), "setslave") then
        awful.client.setslave(c)
    end
    --
    -- New floating windows don't cover the wibox and don't overlap until it's unavoidable
--    awful.placement.no_offscreen(c)
--    awful.placement.no_overlap(c)
    --
    -- Honoring size hints: false to remove gaps between windows
--    c.size_hints_honor = false
end)
--
-- Hook function to execute when arranging the screen
awful.hooks.arrange.register(function (screen)
    -- Update layout imagebox widget with an icon indicating active layout
    local layout = awful.layout.getname(awful.layout.get(screen))
    if layout and beautiful["layout_" ..layout] then
        mylayoutbox[screen].image = image(beautiful["layout_" .. layout])
    else
        mylayoutbox[screen].image = nil
    end
    --
    -- Give focus to the latest client in history if no window has focus
    -- or if the current window is a desktop or a dock one
    if not client.focus then
        local c = awful.client.focus.history.get(screen, 0)
        if c then client.focus = c end
    end
    --
    -- Don't draw the border if there is only one client visible
    local tiled_clients = awful.client.tiled(screen)
    if #tiled_clients > 0 then
        if (#tiled_clients == 1) or (layout == 'max') then
            tiled_clients[1].border_width = 0
        else
            for unused, current in pairs(tiled_clients) do
                current.border_width = beautiful.border_width
                -- Floating clients always on top
                --current:lower()
            end
        end
    end
end)
-- }}}
