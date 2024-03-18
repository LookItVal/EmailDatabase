import numpy as np

class Database:
  # DUNDER METHODS
  def __init__(self) -> None:
    # Why am I using numpy arrays for this?
    # Static Typing.
    # Low Level Memory Management.
    # Numpy will calculate the size of the array without me having to write a function.
    self.__data = [];