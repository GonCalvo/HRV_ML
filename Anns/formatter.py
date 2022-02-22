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
        # get rid of the header
        possible_types = {}
        file_read.readline()
        for line in file_read:
            data = line.split()

            time = data[0]
            minutes = int(time[0:time.index(":")])
            seconds = int(time[time.index(":") + 1:time.index(".")]) + minutes * 60
            milliseconds = int(time[time.index(".") + 1:]) + seconds * 1000

            time_delta = milliseconds - prev_time
            prev_time = milliseconds

            # Removing some outliers caused on the first beat happening too early
            if milliseconds < 150:
                continue

            # data[2] is the type
            type = data[2]
            if type in possible_types:
                possible_types[type] += 1
            else:
                possible_types[type] = 1

            # Should be true and false, but for easier cross-language, will use 1 and 0
            should_be_filtered = 1
            if type in IMPORTANT_TYPES:
                should_be_filtered = 0

            if file_write is not None:
                file_write.write(f"{time_delta}\t{should_be_filtered}\t{milliseconds}\n")

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


'''
def main():
    files = os.listdir(os.getcwd())
    format_file(files[5])
'''

if __name__ == "__main__":
    main()
