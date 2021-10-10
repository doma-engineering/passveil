set -l commands show search list info insert edit move delete allow deny distrust init sync undo

function __passveil_needs_store
  set -l cmd (commandline -poc)

  if test "$cmd[-1]" = "--store"
    return 0
  else
    return 1
  end
end

function __passveil_needs_path
  set -l cmd (commandline -poc)
  set -l paths (__passveil_list)
  set -e cmd[1]

  for c in $cmd[-1..1]
    switch $c
      case '-*'
        continue
    end

    for p in $paths
      if test "$c" = "$p"
        return 1
      end
    end

    return 0
  end

  return 0
end

function __passveil_list
  set -l cmd (commandline -poc)
  set -e cmd[1]
  set -l found 1

  for i in $cmd
    if test $found -eq 0
      passveil --store $i list
      return
    end

    if test "$i" = "--store"
      set found 0
      continue
    end
  end

  passveil list
end

function __passveil_paths
  for p in (__passveil_list)
    echo (dirname $p)"/"
  end
end

complete -c passveil -f

complete -c passveil -n "__passveil_needs_store && not __fish_seen_subcommand_from $commands" -x -a "(__fish_complete_directories (commandline -ct))"
complete -c passveil -n "not __passveil_needs_store && not __fish_seen_subcommand_from $commands" -x -a "$commands"

complete -c passveil -n "__fish_seen_subcommand_from show info edit move delete allow deny list && __passveil_needs_path" -x -a "(__passveil_list)"
complete -c passveil -n "__fish_seen_subcommand_from insert && __passveil_needs_path" -x -a "(__passveil_paths)"
complete -c passveil -n "__fish_seen_subcommand_from allow deny && not __passveil_needs_path" -x -a "(__fish_complete_gpg_user_id gpg2)"
complete -c passveil -n "__fish_seen_subcommand_from move && not __passveil_needs_path" -x -a "(__passveil_paths)"
complete -c passveil -n "__fish_seen_subcommand_from init" -x -a "(__fish_complete_gpg_user_id gpg2)"
complete -c passveil -n "__fish_seen_subcommand_from distrust" -x -a "(__fish_complete_gpg_user_id gpg2)"

complete -c passveil -n "not __fish_seen_subcommand_from $commands" -l store
complete -c passveil -n "not __fish_seen_subcommand_from $commands" -l version
complete -c passveil -s h -l help
complete -c passveil -n "__fish_seen_subcommand_from init" -l untracked
complete -c passveil -n "__fish_seen_subcommand_from init" -l unsigned
complete -c passveil -n "__fish_seen_subcommand_from insert edit" -l generate
complete -c passveil -n "__fish_seen_subcommand_from list search" -l tree
