# Identify the current device

function is_work_machine {
  [[ -d "/usr/local/google/home/cashweaver" ]]
}

function is_work_laptop {
  [ is_work_machine ] && [ inxi --machine | grep -q "Type: Laptop" ]
}

function is_work_desktop {
  [ is_work_machine ] && [ inxi --machine | grep -q "Type: Desktop" ]
}

function is_work_cloudtop {
  [ is_work_machine ] && [ inxi --machine | grep -q "Type: Kvm" ]
}
