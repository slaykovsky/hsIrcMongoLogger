import re

cmd = re.compile("Command: ([A-Z]+)")
rpl = re.compile("([0-9]{3}).*(RPL_[A-Z]+[0-9]*)")
err = re.compile("([0-9]{3}).*(ERR_[A-Z]+)")

with open("IRC.txt", "r") as f:
    with open("src/CommandsTable.hs", "w") as ct:
        ct.write("{-# LANGUAGE OverloadedStrings #-}\n")
        ct.write("\n")
        ct.write("module CommandsTable (commandsTable) where\n")
        ct.write("\n")
        ct.write("import IrcCommand\n")
        ct.write("\n")
        ct.write("commandsTable :: [(String, IrcCommand)]\n")
        ct.write("commandsTable = [\n")
        lines = f.read()
        res = set(sorted(re.findall(cmd, lines)))
        res = zip(res, map(lambda x: "Cmd_{}".format(x.capitalize()), res))

        def write_ct(fd, line):
            fd.write(
                '  , ("{0}", {1})\n'.format(
                    line[0], line[1].title().replace("_", "")))

        def write_ic(fd, line):
            fd.write(
                '                | {}\n'.format(
                    line[1].title().replace("_", "")))

        with open("src/IrcCommand.hs", "w") as ic:
            line = res[0]

            ic.write("module IrcCommand where\n")
            ic.write("\n")
            ic.write(
                "data IrcCommand = {command}\n".format(
                    command=line[1].title().replace("_", "")))

            ct.write(
                '    ("{string}", {command})\n'.format(
                    string=line[0], command=line[1].title().replace("_", "")))

            for line in res[1:]:
                write_ct(ct, line)
                write_ic(ic, line)

            res = set(sorted(re.findall(rpl, lines)))
            for line in res:
                write_ct(ct, line)
                write_ic(ic, line)

            res = set(sorted(re.findall(err, lines)))
            for line in res:
                write_ct(ct, line)
                write_ic(ic, line)

            ic.write("                  deriving (Eq, Show)\n")

        ct.write("  ]\n")
