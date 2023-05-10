import os
from contextlib import contextmanager

SUBFOLDER = "formatted"
IMPORTANT_TYPES = ["R", "F", "N"]


@contextmanager
def open_if(file, condition: bool, mode: str):
    if condition:
        yield open(file, mode)
    else:
        yield None


def format_file(file: str) -> dict:
    file_write_name = SUBFOLDER + "/" + file[:-4] + ".txt"
    with open(file) as file_read, open_if(file_write_name, not os.path.isfile(file_write_name), "x") as file_write:
        prev_time = 0
        # get rid of the header and first record
        lines = file_read.readlines()[2:]
        possible_types = {}
        prev_instance_was_ventricular = False
        for line in lines:

            data = line.split()

            time = data[0]
            minutes = int(time[0:time.index(":")])
            seconds = int(time[time.index(":") + 1:time.index(".")]) + minutes * 60
            milliseconds = int(time[time.index(".") + 1:]) + seconds * 1000

            time_delta = milliseconds - prev_time
            prev_time = milliseconds

            # data[2] is the type
            type = data[2]
            if type in possible_types:
                possible_types[type] += 1
            else:
                possible_types[type] = 1


            should_be_filtered = 1
            if prev_instance_was_ventricular:
                # Now it can be another ventricular or normal
                if type in IMPORTANT_TYPES:
                    should_be_filtered = 2 # This is a normal beat a ventricular beat
                    prev_instance_was_ventricular = False
                else:
                    should_be_filtered = 3 # Ventricular beat after ventricular beat
            elif type in IMPORTANT_TYPES:
                should_be_filtered = 0
                prev_instance_was_ventricular = False
            else:
                prev_instance_was_ventricular = True

            if file_write is not None:
                file_write.write(f"{time_delta}\t{should_be_filtered}\n")

    print(f"\tTypes found in file \"{file}\": {possible_types}")
    return possible_types


def main():
    files = os.listdir(os.getcwd())
    types = {}
    file_info = {}
    report_name = "Type_report.txt"
    for file in files:
        if file.endswith(".txt") and file != report_name:
            print(f"Formatting file: {file}")
            types_in_file = format_file(file)
            for t in types_in_file.keys():
                if t in types:
                    types[t][file] = types_in_file[t]
                else:
                    types[t] = {file: types_in_file[t]}
            file_info[file] = types_in_file

    with open_if(report_name, not os.path.isfile(report_name), "x") as fw:
        if fw is not None:
            fw.write(f"We found these types between in each files:\n")
            for file in file_info:
                fw.write(f"\tIn file \'{file}\' we found: {file_info[file]}\n")

            fw.write(f"We found these types between all files:\n")
            for t in types.keys():
                total_times = sum([types[t][file] for file in types[t]])
                fw.write(f"\tType \'{t}\': Found {total_times} times\n")
                for file in types[t]:
                    fw.write(f"\t\tFound {types[t][file]} times in file \'{file}\'\n")


if __name__ == "__main__":
    main()
