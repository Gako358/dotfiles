/* See LICENSE file for copyright and license details. */

#include <X11/XF86keysym.h>

/* appearance */
static const unsigned int borderpx          = 0; /* border pixel of windows */
static const unsigned int default_border    = 0; /* to switch back to default border after dynamic border resizing via keybinds */
static const unsigned int snap              = 32;   /* snap pixel */
static const unsigned int gappih            = 19; /* horiz inner gap between windows */
static const unsigned int gappiv            = 19; /* vert inner gap between windows */
static const unsigned int gappoh            = 19; /* horiz outer gap between windows and screen edge */
static const unsigned int gappov            = 19; /* vert outer gap between windows and screen edge */
static const int smartgaps                  = 0; /* 1 means no outer gap when there is only one window */
static const unsigned int systraypinning    = 0; /* 0: sloppy systray follows selected monitor, >0: pin systray to monitor X */
static const unsigned int systrayspacing    = 2; /* systray spacing */
static const int systraypinningfailfirst    = 1; /* 1: if pinning fails,display systray on the 1st monitor,False: display systray on last monitor*/
static const int showsystray        = 1; /* 0 means no systray */
static const int showbar            = 1; /* 0 means no bar */
static const int showtab            = showtab_auto;
static const int toptab             = 1; /* 0 means bottom tab */
static const int topbar             = 1; /* 0 means bottom bar */
static const int horizpadbar        = 5;
static const int vertpadbar         = 11;
static const int vertpadtab         = 33;
static const int horizpadtabi       = 15;
static const int horizpadtabo       = 15;
static const int scalepreview       = 4;
static const int tag_preview        = 1; /* 1 means enable, 0 is off */
static const int colorfultag        = 1; /* 0 means use SchemeSel for selected non vacant tag */

static const char *fonts[] = {"JetBrainsMono Nerd Font:style:medium:size=10",
                              "Material Design Icons-Regular:size=11"};

// theme
static const char black[]   = "#282828";
static const char gray2[]   = "#2e323a"; // unfocused window border
static const char gray3[]   = "#545862";
static const char gray4[]   = "#6d8dad";
static const char blue[]    = "#81A1C1"; // focused window border
static const char green[]   = "#89b482";
static const char red[]     = "#d57780";
static const char orange[]  = "#caaa6a";
static const char yellow[]  = "#EBCB8B";
static const char pink[]    = "#e39a83";
static const char cyan[]    = "#56b6c2";
static const char diff[]    = "#87cc54";
static const char col_borderbar[] = "#282828"; // inner border

static const char *colors[][3] = {
    /*                     fg       bg      border */
    [SchemeNorm]        = {gray3, black, gray2},
    [SchemeSel]         = {gray4, blue, blue},
    [TabSel]            = {blue, gray2, black},
    [TabNorm]           = {gray3, black, black},
    [SchemeTag]         = {gray3, black, black},
    [SchemeTag1]        = {blue, black, black},
    [SchemeTag2]        = {diff, black, black},
    [SchemeTag3]        = {cyan, black, black},
    [SchemeTag4]        = {orange, black, black},
    [SchemeTag5]        = {red, black, black},
    [SchemeTag6]        = {yellow, black, black},
    [SchemeTag7]        = {pink, black, black},
    [SchemeLayout]      = {green, black, black},
    [SchemeBtnPrev]     = {green, black, black},
    [SchemeBtnNext]     = {yellow, black, black},
    [SchemeBtnClose]    = {red, black, black},
};

typedef struct {
  const char *name;
  const void *cmd;
} Sp;
const char *spcmd1[] = {"st", "-n", "spterm",  "-g", "172x46",                      NULL};
const char *spcmd2[] = {"st", "-n", "ncmpcpp", "-g", "128x37", "-e", "ncspot",      NULL};
const char *spcmd3[] = {"st", "-n", "mutt",    "-g", "172x46", "-e", "neomutt",     NULL};
const char *spcmd4[] = {"st", "-n", "htop",    "-g", "144x46", "-e", "btop",        NULL};
const char *spcmd5[] = {"st", "-n", "weechat", "-g", "172x46", "-e", "weechat",     NULL};
const char *spcmd6[] = {"st", "-n", "ranger",  "-g", "172x46", "-e", "ranger",      NULL};
static Sp scratchpads[] = {
    /* name          cmd  */
    {"spterm", spcmd1}, {"ncmpcpp", spcmd2}, {"mutt", spcmd3},
    {"htop", spcmd4},   {"weechat", spcmd5}, {"ranger", spcmd6},
};

/* tagging */
static char *tags[] = {"", "", "", "", "", "", ""};

static const int tagschemes[] = {
    SchemeTag1, SchemeTag2, SchemeTag3, SchemeTag4,
    SchemeTag5, SchemeTag6, SchemeTag7,
};

static const unsigned int ulinepad      = 5; /* horizontal padding between the underline and tag */
static const unsigned int ulinestroke   = 2; /* thickness / height of the underline */
static const unsigned int ulinevoffset  = 0; /* how far above the bottom of the bar the line should appear */
static const int ulineall               = 0; /* 1 to show underline on all tags, 0 for just the active ones */

static const Rule rules[] = {
    /* xprop(1):
     *	WM_CLASS(STRING) = instance, class
     *	WM_NAME(STRING) = title
     */
    /* class      instance    title       tags mask     iscentered   isfloating
       monitor */
    {"Gimp",                NULL, NULL, 0, 1, 1, -1},
    {"firefox",             NULL, NULL, 1, 0, 0,  2},
    {"thunderbird",         NULL, NULL, 1 << 5, 0, 0,  1},
    {"Geany",               NULL, NULL, 0, 0, 1, -1},
    {"Pcmanfm",             NULL, NULL, 0, 0, 1, -1},
    {"Zathura",             NULL, NULL, 0, 0, 1, -1},
    {"Wfica",               NULL, NULL, 1 << 4, 0, 0,  2},

    // Chromium Apps
    {NULL, "discord.com__channels_@me",                     NULL, 1 << 6, 0, 0,  1},
    {NULL, "app.slack.com__client_T04MZPW21RA_C04MUBWKREZ", NULL, 1 << 6, 0, 0,  2},
    // Microsoft Edge
    {"Microsoft-edge-dev",                                  NULL, NULL, 1, 0, 0,  1},
    // Citrix
    {"Remote Desktop Connection",                           NULL, NULL, 1 << 4, 0, 1,  2},

    {NULL, "spterm",  NULL, SPTAG(0), 0, 1, -1},
    {NULL, "ncmpcpp", NULL, SPTAG(1), 0, 1, -1},
    {NULL, "mutt",    NULL, SPTAG(2), 0, 1, -1},
    {NULL, "htop",    NULL, SPTAG(3), 0, 1, -1},
    {NULL, "weechat", NULL, SPTAG(4), 0, 1, -1},
    {NULL, "ranger",  NULL, SPTAG(5), 0, 1, -1},
};

/* layout(s) */
static const float mfact        = 0.55; /* factor of master area size [0.05..0.95] */
static const int nmaster        = 1; /* number of clients in master area */
static const int resizehints    = 0; /* 1 means respect size hints in tiled resizals */
static const int lockfullscreen = 1; /* 1 will force focus on the fullscreen window */

#define FORCE_VSPLIT                                                           \
  1 /* nrowgrid layout: force two clients to always split vertically */
#include "functions.h"

static const Layout layouts[] = {
    /* symbol     arrange function */
    {"[@]", spiral},
    {"[]=", tile}, /* first entry is default */
    {"|M|", centeredmaster},
    {"[M]", monocle},
    {"[\\]", dwindle},
    {"H[]", deck},
    {"TTT", bstack},
    {"===", bstackhoriz},
    {"HHH", grid},
    {"###", nrowgrid},
    {"---", horizgrid},
    {":::", gaplessgrid},
    {">M>", centeredfloatingmaster},
    {"><>", NULL}, /* no layout function means floating behavior */
};

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY, TAG)                                                      \
  {MODKEY, KEY, view, {.ui = 1 << TAG}},                                       \
      {MODKEY | ControlMask, KEY, toggleview, {.ui = 1 << TAG}},               \
      {MODKEY | ShiftMask, KEY, tag, {.ui = 1 << TAG}},                        \
      {MODKEY | ControlMask | ShiftMask, KEY, toggletag, {.ui = 1 << TAG}},

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd)                                                             \
  {                                                                            \
    .v = (const char *[]) { "/bin/sh", "-c", cmd, NULL }                       \
  }

/* commands */
static Key keys[] = {
    /* modifier                         key         function        argument */

    // brightness and audio
    {0, XF86XK_AudioMute, spawn, SHCMD("amixer -D pulse set Master 1+ toggle")},
    {0, XF86XK_AudioNext, spawn, SHCMD("mpc next")},
    {0, XF86XK_AudioPrev, spawn, SHCMD("mpc prev")},
    {0, XF86XK_AudioPlay, spawn, SHCMD("mpc play")},
    {0, XF86XK_AudioStop, spawn, SHCMD("mpc pause")},
    {0, XF86XK_AudioRaiseVolume, spawn, SHCMD("amixer -c 0 sset Master 5%+")},
    {0, XF86XK_AudioLowerVolume, spawn, SHCMD("amixer -c 0 sset Master 5%-")},
    {0, XF86XK_MonBrightnessDown, spawn, SHCMD("xbacklight -dec 5")},
    {0, XF86XK_MonBrightnessUp, spawn, SHCMD("xbacklight -inc 5")},

    // screenshot fullscreen and cropped
    {MODKEY | ControlMask, XK_u, spawn,
     SHCMD("maim | xclip -selection clipboard -t image/png")},
    {MODKEY, XK_u, spawn,
     SHCMD("maim --select | xclip -selection clipboard -t image/png")},

    // Apps
    {MODKEY, XK_c, spawn, SHCMD("rofi -show drun")},
    {MODKEY | ShiftMask, XK_q, spawn,
     SHCMD("rofi -show power-menu -modi power-menu:rofi-power-menu")},
    {MODKEY, XK_Return, spawn, SHCMD("st")},
    {MODKEY, XK_w, spawn, SHCMD("pcmanfm")},
    {MODKEY | ShiftMask, XK_l, spawn, SHCMD("slock")},
    {MODKEY | ControlMask | ShiftMask, XK_p, spawn,
     SHCMD("scrot -d3 /home/merrinx/Pictures/Screenshots/")},

    // Window opacity
    {MODKEY | ControlMask, XK_a, spawn, SHCMD("picom-trans -c -10")},
    {MODKEY | ControlMask | ShiftMask, XK_a, spawn,
     SHCMD("picom-trans -c +100")},

    // Languages
    {MODKEY | ControlMask | ShiftMask, XK_n, spawn,
     SHCMD("setxkbmap -layout no")},
    {MODKEY | ControlMask | ShiftMask, XK_u, spawn,
     SHCMD("setxkbmap -layout us")},

    // Suspend
    {MODKEY | ShiftMask, XK_p, spawn, SHCMD("systemctl suspend")},

    // scratchpads
    {MODKEY,                XK_grave, togglescratch,    {.ui = 0}}, // terminal
    {MODKEY | ShiftMask,    XK_n, togglescratch,        {.ui = 1}}, // btop
    {MODKEY | ShiftMask,    XK_m, togglescratch,        {.ui = 2}}, // weechat
    {MODKEY | ShiftMask,    XK_b, togglescratch,        {.ui = 3}}, // ncmpcpp
    {MODKEY | ShiftMask,    XK_c, togglescratch,        {.ui = 4}}, // cmus
    {MODKEY,                XK_r, togglescratch,        {.ui = 5}}, // ranger

    // toggle stuff
    {MODKEY, XK_b, togglebar, {0}},
    {MODKEY | ControlMask, XK_t, togglegaps, {0}},
    {MODKEY | ShiftMask, XK_space, togglefloating, {0}},
    {MODKEY, XK_f, togglefullscr, {0}},

    {MODKEY | ControlMask, XK_w, tabmode, {-1}},
    {MODKEY, XK_j, focusstack, {.i = +1}},
    {MODKEY, XK_k, focusstack, {.i = -1}},
    {MODKEY, XK_i, incnmaster, {.i = +1}},
    {MODKEY, XK_d, incnmaster, {.i = -1}},

    // change m,cfact sizes
    {MODKEY, XK_h, setmfact, {.f = -0.05}},
    {MODKEY, XK_l, setmfact, {.f = +0.05}},
    {MODKEY | ShiftMask, XK_h, setcfact, {.f = +0.25}},
    {MODKEY | ShiftMask, XK_l, setcfact, {.f = -0.25}},
    {MODKEY | ShiftMask, XK_o, setcfact, {.f = 0.00}},

    {MODKEY | ShiftMask, XK_j, movestack, {.i = +1}},
    {MODKEY | ShiftMask, XK_k, movestack, {.i = -1}},
    {MODKEY | ShiftMask, XK_Return, zoom, {0}},
    {MODKEY, XK_Tab, view, {0}},

    // overall gaps
    {MODKEY | ControlMask, XK_i, incrgaps, {.i = +1}},
    {MODKEY | ControlMask, XK_d, incrgaps, {.i = -1}},

    // inner gaps
    {MODKEY | ShiftMask, XK_i, incrigaps, {.i = +1}},
    {MODKEY | ControlMask | ShiftMask, XK_i, incrigaps, {.i = -1}},

    // outer gaps
    {MODKEY | ControlMask, XK_o, incrogaps, {.i = +1}},
    {MODKEY | ControlMask | ShiftMask, XK_o, incrogaps, {.i = -1}},

    // inner+outer hori, vert gaps
    {MODKEY | ControlMask, XK_6, incrihgaps, {.i = +1}},
    {MODKEY | ControlMask | ShiftMask, XK_6, incrihgaps, {.i = -1}},
    {MODKEY | ControlMask, XK_7, incrivgaps, {.i = +1}},
    {MODKEY | ControlMask | ShiftMask, XK_7, incrivgaps, {.i = -1}},
    {MODKEY | ControlMask, XK_8, incrohgaps, {.i = +1}},
    {MODKEY | ControlMask | ShiftMask, XK_8, incrohgaps, {.i = -1}},
    {MODKEY | ControlMask, XK_9, incrovgaps, {.i = +1}},
    {MODKEY | ControlMask | ShiftMask, XK_9, incrovgaps, {.i = -1}},

    {MODKEY | ControlMask | ShiftMask, XK_d, defaultgaps, {0}},

    // layout
    {MODKEY, XK_t, setlayout, {.v = &layouts[0]}},
    {MODKEY | ShiftMask, XK_f, setlayout, {.v = &layouts[1]}},
    {MODKEY, XK_m, setlayout, {.v = &layouts[2]}},
    {MODKEY | ControlMask, XK_g, setlayout, {.v = &layouts[10]}},
    {MODKEY | ControlMask | ShiftMask, XK_t, setlayout, {.v = &layouts[13]}},
    {MODKEY, XK_space, setlayout, {0}},
    {MODKEY | ControlMask, XK_comma, cyclelayout, {.i = -1}},
    {MODKEY | ControlMask, XK_period, cyclelayout, {.i = +1}},
    {MODKEY, XK_0, view, {.ui = ~0}},
    {MODKEY | ShiftMask, XK_0, tag, {.ui = ~0}},
    {MODKEY, XK_comma, focusmon, {.i = -1}},
    {MODKEY, XK_period, focusmon, {.i = +1}},
    {MODKEY | ShiftMask, XK_comma, tagmon, {.i = -1}},
    {MODKEY | ShiftMask, XK_period, tagmon, {.i = +1}},

    // change border size
    {MODKEY | ShiftMask, XK_minus, setborderpx, {.i = -1}},
    {MODKEY | ShiftMask, XK_p, setborderpx, {.i = +1}},
    {MODKEY | ShiftMask, XK_w, setborderpx, {.i = default_border}},

    // kill dwm
    {MODKEY | ControlMask, XK_q, spawn, SHCMD("killall bar.sh dwm")},

    // kill window
    {MODKEY, XK_q, killclient, {0}},

    // restart
    {MODKEY | ShiftMask, XK_r, restart, {0}},

    // hide & restore windows
    {MODKEY, XK_e, hidewin, {0}},
    {MODKEY | ShiftMask, XK_e, restorewin, {0}},

    TAGKEYS(XK_1, 0) TAGKEYS(XK_2, 1) TAGKEYS(XK_3, 2) TAGKEYS(XK_4, 3)
        TAGKEYS(XK_5, 4) TAGKEYS(XK_6, 5) TAGKEYS(XK_7, 6) TAGKEYS(XK_8, 7)
            TAGKEYS(XK_9, 8)};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle,
 * ClkClientWin, or ClkRootWin */
static Button buttons[] = {
    /* click                event mask      button          function argument */
    {ClkLtSymbol, 0, Button1, setlayout, {0}},
    {ClkLtSymbol, 0, Button3, setlayout, {.v = &layouts[2]}},
    {ClkWinTitle, 0, Button2, zoom, {0}},
    {ClkStatusText, 0, Button2, spawn, SHCMD("st")},

    /* Keep movemouse? */
    /* { ClkClientWin,         MODKEY,         Button1,        movemouse, {0} },
     */

    /* placemouse options, choose which feels more natural:
     *    0 - tiled position is relative to mouse cursor
     *    1 - tiled postiion is relative to window center
     *    2 - mouse pointer warps to window center
     *
     * The moveorplace uses movemouse or placemouse depending on the floating
     * state of the selected client. Set up individual keybindings for the two
     * if you want to control these separately (i.e. to retain the feature to
     * move a tiled window into a floating position).
     */
    {ClkClientWin, MODKEY, Button1, moveorplace, {.i = 0}},
    {ClkClientWin, MODKEY, Button2, togglefloating, {0}},
    {ClkClientWin, MODKEY, Button3, resizemouse, {0}},
    {ClkClientWin, ControlMask, Button1, dragmfact, {0}},
    {ClkClientWin, ControlMask, Button3, dragcfact, {0}},
    {ClkTagBar, 0, Button1, view, {0}},
    {ClkTagBar, 0, Button3, toggleview, {0}},
    {ClkTagBar, MODKEY, Button1, tag, {0}},
    {ClkTagBar, MODKEY, Button3, toggletag, {0}},
    {ClkTabBar, 0, Button1, focuswin, {0}},
    {ClkTabBar, 0, Button1, focuswin, {0}},
    {ClkTabPrev, 0, Button1, movestack, {.i = -1}},
    {ClkTabNext, 0, Button1, movestack, {.i = +1}},
    {ClkTabClose, 0, Button1, killclient, {0}},
};
