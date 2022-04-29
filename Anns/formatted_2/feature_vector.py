import argparse
import os
from contextlib import contextmanager


class WindowError(Exception):
    pass

@contextmanager
def open_if(file, condition: bool, mode: str):
    if condition:
        yield open(file, mode)
    else:
        yield None

def map_beat_type(beat_type: str) -> str:
    return beat_type


def get_feature_vector(beat_window: list, beat_type: str | int, reference_beat: int = -1) -> list:
    """
    :param beat_window: Window of times (relative to the beginning of the measurement) of the beats.
    :param beat_type: The type of beat we are analyzing, can be a string and be mapped or it can come mapped already
    :param reference_beat: Beat from which to make the references.
    :return: A list of time deltas between the middle value and the rest.
    """
    if type(beat_type) == str:
        beat_type = map_beat_type(beat_type)

    window_size = len(beat_window)
    if not (window_size % 2) and reference_beat == -1:
        raise WindowError("The window doesn't have a middle measurement to calculate the delta's from and none was "
                          "specified.")

    if reference_beat == -1:
        reference_beat = int(window_size / 2)

    print(f"{reference_beat = }\n{beat_window = }\n{beat_type = }")

    #del beat_window[reference_beat]
    feature_vector = beat_window

    # for i, _ in enumerate(beat_window):
    #     if i == reference_beat:
    #         continue
    #
    #     if i < reference_beat:
    #         time_delta = sum(beat_window[i:reference_beat])
    #     else:
    #         time_delta = sum(beat_window[reference_beat+1:i+1])
    #
    #     feature_vector.append(str(time_delta))

    return feature_vector + [beat_type]


def process_file(file: str, window_bkw: int, window_fwd: int) -> None:
    print(f"Processing file: {file}")
    directory = f"b{window_bkw}_f{window_fwd}"
    if not os.path.isdir(directory):
        os.mkdir(directory)

    file_write = f"{directory}/{file}"
    with open(file) as file_read, open_if(file_write, not os.path.isfile(file_write), "x") as file_write:
        if file_write is None:
            return

        lines = file_read.readlines()

        # Next line writes the header, as i+n where n is the different indexes and finally the type,
        # all of it separated by tabs
        file_write.write("\t".join([f"i{i:+}" for i in range(-window_bkw, window_fwd + 1)] + ["Filter"]) + "\n")

        for i, line in enumerate(lines):
            if i < window_bkw:
                continue
            if i >= (len(lines) - window_bkw):
                break

            window_lines = lines[i - window_bkw:i + window_fwd + 1]
            feature_vector = [int(l.split()[0]) for l in window_lines] + line.split()[1:]
            #feature_vector = get_feature_vector(beat_window=window_times, beat_type=line.split()[1],reference_beat=window_bkw)
            file_write.write("\t".join(str(feature) for feature in feature_vector) + "\n")


def main():
    parser = argparse.ArgumentParser(description="Create a file of feature vectors from a file of heartbeat measures")
    parser.add_argument(
        "-F",
        "--file",
        required=False,
        default="",
        help="Specify the file to calculate the feature vectors from. If none is specified, all the files "
             "in the folder will be evaluated."
    )
    parser.add_argument(
        "-w",
        "--window",
        #required=True,
        default=1,
        type=int,
        help="Specify the window size this number will be the number of beats forward and the number of beat backwards "
             "to take into account to make the feature vector"
    )

    args = parser.parse_args()
    if args.file == "":
        files = os.listdir(os.getcwd())
        for file in files:
            if file.endswith(".txt"):
                process_file(file, args.window, args.window)
    else:
        process_file(args.file, args.window, args.window)


if __name__ == "__main__":
    main()
