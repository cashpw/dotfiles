# Helpful monitor variables

#east_monitor_output_name="DP-1-1"
#north_monitor_output_name="DP-1-3"
#south_monitor_output_name="DP-1-2"
#west_monitor_output_name="DP-2-3"

#primary_monitor_output_name=$south_monitor_output_name

#north_monitor_crtc_number=1
#south_monitor_crtc_number=0
#west_monitor_crtc_number=2
#eDP_1_crtc_number=0

function is_work_computer() {
  [[ -d "/usr/local/google/home/cashweaver" ]]
}

if is_work_computer; then
  left_monitor_output_name="DP-1-3"
  left_monitor_crtc_number=2
  center_monitor_output_name="DP-1-2"
  center_monitor_crtc_number=0
  right_monitor_output_name="DP-1-1"
  right_monitor_crtc_number=1
#else
  # Home configuration
  # TODO
fi
