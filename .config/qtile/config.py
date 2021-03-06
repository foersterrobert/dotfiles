from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
import platform
import os

mod = "mod4"
myTerm = "gnome-terminal"
myBrowser = "brave-browser"
device = platform.node()

keys = [
    Key([mod], "Return", lazy.spawn(myTerm), desc="Launch terminal"),
    Key([mod, "shift"], "Return", lazy.spawn("rofi -show drun"), desc='Run Launcher'),
    
    Key([mod], "c", lazy.spawn("code"), desc="Spawn code"),
    Key([mod], "b", lazy.spawn(myBrowser), desc="Spawn Brave"),
    Key([mod], "f", lazy.spawn("nautilus"), desc="Spawn Files"),
    
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "Tab", lazy.layout.next(), desc="Move window focus to other window"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    # Key(
    #     [mod, "shift"],
    #     "Return",
    #     lazy.layout.toggle_split(),
    #     desc="Toggle between split and unsplit sides of stack",
    # ),
    # Toggle between different layouts as defined below
    Key([mod], "space", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod, "shift"], "c", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),

    # sound
    Key([], "XF86AudioMute", lazy.spawn("pactl set-sink-mute @DEFAULT_SINK@ toggle")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ -5%")),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ +5%")),
]

groups = [Group("1", label="CODE"), Group("2", label="WEB"), Group("3", label="TERM"), Group("4", label="FILES"), Group("5", label="OTHER")]

for i in groups:
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key(
                [mod],
                i.name,
                lazy.group[i.name].toscreen(),
                desc="Switch to group {}".format(i.name),
            ),
            # mod1 + shift + letter of group = switch to & move focused window to group
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name, switch_group=True),
                desc="Switch to & move focused window to group {}".format(i.name),
            ),
        ]
    )

layout_theme = {"border_width": 2,
                "margin": 8,
                "border_focus": "e1acff",
                "border_normal": "1D2330"
                }

layouts = [
    layout.Columns(**layout_theme),
    layout.Max(**layout_theme),
]

colors = [["#282c34", "#282c34"],
          ["#1c1f24", "#1c1f24"],
          ["#dfdfdf", "#dfdfdf"],
          ["#ff6c6b", "#ff6c6b"],
          ["#98be65", "#98be65"],
          ["#da8548", "#da8548"],
          ["#51afef", "#51afef"],
          ["#c678dd", "#c678dd"],
          ["#46d9ff", "#46d9ff"],
          ["#a9a1e1", "#a9a1e1"]]

widget_defaults = dict(
    font="Ubuntu Bold",
    fontsize=12,
    padding=2,
    background="#231e20"
)
extension_defaults = widget_defaults.copy()

def get_widgets():
    widgets = [
        widget.Sep(
            linewidth = 0,
            padding = 6,
            foreground = colors[2],
            background = colors[0]
        ),
        widget.Image(
            filename = "~/.config/qtile/icons/python-white.png",
            scale = "False",
        ),
        widget.Sep(
            linewidth = 0,
            padding = 6,
            foreground = colors[2],
            background = colors[0]
        ),
        widget.GroupBox(
                margin_y = 3,
                margin_x = 0,
                padding_y = 5,
                padding_x = 5,
                borderwidth = 3,
                active = colors[2],
                inactive = colors[7],
                rounded = False,
                highlight_color = colors[1],
                highlight_method = "line",
                this_current_screen_border = colors[6],
                this_screen_border = colors [4],
                other_current_screen_border = colors[6],
                other_screen_border = colors[4],
                foreground = colors[2],
                background = colors[0],
                fontsize = 10,
                ),
        widget.TextBox(
                text = '|',
                font = "Ubuntu Mono",
                background = colors[0],
                foreground = '474747',
                padding = 2,
                fontsize = 14
                ),
        widget.CurrentLayoutIcon(
                custom_icon_paths = [os.path.expanduser("~/.config/qtile/icons")],
                foreground = colors[2],
                background = colors[0],
                padding = 0,
                scale = 0.7
                ),
        widget.CurrentLayout(
                foreground = colors[2],
                background = colors[0],
                padding = 5
                ),
        widget.TextBox(
                text = '|',
                font = "Ubuntu Mono",
                background = colors[0],
                foreground = '474747',
                padding = 2,
                fontsize = 14
                ),
        widget.WindowName(
                foreground = colors[6],
                background = colors[0],
                padding = 0
                ),
        widget.Systray(
                background = colors[0],
                padding = 6
                ),
        widget.Sep(
                linewidth = 0,
                padding = 6,
                foreground = colors[2],
                background = colors[0]
        ),
        widget.PulseVolume(
                foreground = colors[1],
                background = colors[6],
                padding = 5,
            ),
        widget.Clock(
            foreground = colors[1],
            background = colors[7],
            padding = 5,
            format="%d.%m.%Y %a %I:%M %p"
        )
    ]
    if device == 'robert':
        widgets = widgets[:-2] + [widget.Battery(
            foreground = colors[1],
            background = colors[2],
            padding = 4,
            format = '{percent:2.0%}'
        )] + widgets[-2:]
        
    return widgets

screens = [
    Screen(
        wallpaper=os.path.expanduser("~/.config/qtile/wallpaper.jpg"),
        wallpaper_mode="stretch",
        top=bar.Bar(
            widgets=get_widgets(),
            opacity=1.0,
            size=22,
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

wmname = "LG3D"
