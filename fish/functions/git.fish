function tmux_chooser
  if test "$TMUX" != ""
    return
  end

  set session_count (tmux list-sessions | wc -l)
  set output_names (tmux list-sessions -F\#S)
  set i 1
  echo "Choose the session to attach: "
  for session in $output_names
    echo "  $i - $session"
    set i (math $i + 1)
  end
  echo "Or create a new session by entering a name for it"
  read -P '> ' input
  if test "$input" = ""
    tmux
  else if test "$input" = "nil"
    return
  else if begin ;
          string match -r '^[0-9]+$' "$input" > /dev/null ;
          and test "$input" -le "$session_count" ; end
    tmux a -t "$output_names[$input]"
  else
    tmux new -s "$input"
  end
end
