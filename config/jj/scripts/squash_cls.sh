#!/bin/bash
set -euo pipefail

function get_mutable_commit_count() {
  jj log -r 'immutable_heads()..@' --no-graph -T 'commit_id ++ "\n"' | wc -l
}

function get_cl_bookmarks() {
  jj bookmark list -r 'immutable_heads()..@' 'cl/*' -T 'name ++ "\n"'
}

function get_cl_bookmarks_with_descriptions() {
  jj bookmark list -r 'immutable_heads()..@' 'cl/*' -T 'name ++ " - " ++ normal_target.description().first_line() ++ "\n"'
}

function select_bookmark_interactively() {
  local -r names_str="$1"
  local -r display_str="$2"
  local names_array
  local display_array
  mapfile -t names_array <<< "$names_str"
  mapfile -t display_array <<< "$display_str"

  local -r count="${#names_array[@]}"

  if (( count == 0 )); then
    echo ""
    return 0
  fi

  if (( count == 1 )); then
    echo "${names_array[0]}"
    return 0
  fi

  echo "Found the following CL bookmarks in the current chain:" >&2
  for i in "${!display_array[@]}"; do
    echo "$((i+1))) ${display_array[i]}" >&2
  done

  while true; do
    local choice
    read -r -p "Select the CL bookmark to keep (1-$count): " choice >&2
    if [[ "$choice" =~ ^[0-9]+$ ]] && (( choice >= 1 && choice <= count )); then
      echo "${names_array[$((choice-1))]}"
      return 0
    else
      echo "Invalid selection. Please enter a number between 1 and $count." >&2
    fi
  done
}

function filter_unselected_bookmarks() {
  local -r selected="$1"
  shift
  local -r all_bookmarks=("$@")
  local unselected=()

  for bm in "${all_bookmarks[@]}"; do
    if [[ "$bm" != "$selected" ]]; then
      unselected+=("$bm")
    fi
  done

  echo "${unselected[@]}"
}

function main() {
  local -r cl_bookmarks_raw=$(get_cl_bookmarks)

  if [[ -z "$cl_bookmarks_raw" ]]; then
    echo "No cl/* bookmarks found in the current un-submitted chain."
    return 0
  fi

  local cl_array
  mapfile -t cl_array <<< "$cl_bookmarks_raw"

  local -r cl_display_raw=$(get_cl_bookmarks_with_descriptions)

  local -r selected_cl=$(select_bookmark_interactively "$cl_bookmarks_raw" "$cl_display_raw")

  if [[ -z "$selected_cl" ]]; then
    echo "No CL bookmark selected or available."
    return 0
  fi

  echo "Selected CL to keep: $selected_cl"

  local -r unselected_str=$(filter_unselected_bookmarks "$selected_cl" "${cl_array[@]}")
  local unselected_cls=()
  if [[ -n "$unselected_str" ]]; then
    read -r -a unselected_cls <<< "$unselected_str"
  fi

  # Dropping unselected CLs before squashing prevents AmbiguousChangeError in jj piper.
  # keep-local ensures local commit contents remain available for the subsequent squash.
  if [[ ${#unselected_cls[@]} -gt 0 ]]; then
    echo "Dropping unselected CLs from Piper: ${unselected_cls[*]}"
    jj piper cls drop --keep-local "${unselected_cls[@]}"
  fi

  local -r commit_count=$(get_mutable_commit_count)

  # Only squash if there are multiple commits in the chain.
  if (( commit_count > 1 )); then
    echo "Squashing $commit_count commits in the chain into the base commit..."
    jj squash --from 'children(roots(immutable_heads()..@))::@' --into 'roots(immutable_heads()..@)'
  else
    echo "Only 1 commit in the chain; no squashing needed."
  fi

  echo "Setting working-copy commit (@) to $selected_cl..."
  jj edit "$selected_cl"

  echo "Workflow complete. Working copy (@) is now tracking: $selected_cl"
  echo ""
  jj
}

main "$@"
