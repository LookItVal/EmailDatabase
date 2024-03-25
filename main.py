from database import Database
import numpy as np

def example1():
  # This initializes the database object.
  db = Database()
  
  # This is the function that will take the data and store it in the database.
  def store(self: Database, entries):
    data = []
    for row in entries:
      newRow = ['Hello, how are you ', row[0], row[1]]
      data.append(newRow)
    self.data = np.array(data, dtype="S35")

  # This is the function that will generate the emails from the data in the database.
  def generate(self: Database):
    emails = []
    for row in self.data:
      emails.append([row[0]+row[1], row[2]])
    return emails

  # This will link everything up with our database object.
  db.storageFunction = store.__get__(db, Database)
  db.generateFunction = generate.__get__(db, Database)
  # Here you will decide based on your generate algorithm how long it should take to generate the emails.
  db.generationTime = 1
  
  # This will run the tests to make sure everything is working correctly.
  # By default it will generate 10 iterations of 1000 emails with different combinations of names for the entries.
  # If any one test fails, it will return with a profit of 0.
  # If all tests pass, it will return with the average calculated profit for each iteration of the tests.
  db.calculateProfits()


def example2():
  db = Database()

  # This is only different in that it uses a different data type for the data.
  def store(self: Database, entries):
    data_types = [('Email Content', 'S19'), ('Name', 'S30'), ('Email', 'S35')]
    data = []
    for row in entries:
      newRow = ('Hello, how are you ', row[0], row[1])
      data.append(newRow)
    self.data = np.array(data, dtype=data_types)

  def generate(self: Database):
    emails = []
    for row in self.data:
      emails.append([row[0]+row[1], row[2]])
    return emails
  
  db.storageFunction = store.__get__(db, Database)
  db.generateFunction = generate.__get__(db, Database)
  db.generationTime = 1

  db.calculateProfits()


# This is just basic python boilerplate below.
def main():
  example2();


if __name__ == "__main__":
  main()
