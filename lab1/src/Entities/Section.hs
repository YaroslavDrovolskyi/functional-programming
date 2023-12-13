{-# LANGUAGE DerivingStrategies, DeriveAnyClass, GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings #-}

module Entities.Section where

import Entities.SectionStudent
import Entities.SectionInstructor
import Entities.Student
import Entities.Instructor
import Database.PostgreSQL.Simple
import Data.Time.Calendar.OrdinalDate
import Data.Time.LocalTime
import Data.Int(Int64)
import GHC.Generics(Generic)

data Section = Section {
    id :: Int64,
    title :: String,
    description :: String
}
  deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)

getSection :: Connection -> Int64 -> IO[Section]
getSection conn id = query conn "SELECT * FROM sections WHERE id = ?" $ (Only id)

getAllSections :: Connection -> IO[Section]
getAllSections conn = query_ conn "SELECT * FROM sections"

createSection :: Connection -> String -> String -> IO[Only Int64]
createSection conn title description =
  query conn "INSERT INTO sections(title, description) \
              \ VALUES (?, ?) RETURNING id"
              $ ((title, description))

updateSectionTitle :: Connection -> Int64 -> String -> IO(Bool)
updateSectionTitle conn id title = do
  n <- execute conn "UPDATE sections SET title = ? WHERE id = ?" (title, id)
  return $ n > 0

updateSectionDescription :: Connection -> Int64 -> String -> IO(Bool)
updateSectionDescription conn id description = do
  n <- execute conn "UPDATE sections SET description = ? WHERE id = ?" (description, id)
  return $ n > 0

deleteSection :: Connection -> Int64 -> IO(Bool)
deleteSection conn id = do
  n <- execute conn "DELETE FROM sections WHERE id = ?" $ (Only id)
  return $ n > 0



{- Requests connected with Section's students and instructors -}

-- Before applying this function YOU need to check if student is already added to this section
addStudentToSection :: Connection -> Int64 -> Int64 -> IO[Only Int64]
addStudentToSection conn sectionId studentId =
  query conn "INSERT INTO sections_students(student_id, section_id) \
                \ VALUES (?, ?) RETURNING id"
                $ ((studentId, sectionId))

-- Before applying this function YOU need to check if instructor is already added to this section
addInstructorToSection :: Connection -> Int64 -> Int64 -> IO[Only Int64]
addInstructorToSection conn sectionId instructorId =
  query conn "INSERT INTO sections_instructors(instructor_id, section_id) \
                \ VALUES (?, ?) RETURNING id"
                $ ((instructorId, sectionId))


-- Before applying this function YOU need to check if student is really added to this section
removeStudentFromSection :: Connection -> Int64 -> Int64 -> IO(Bool)
removeStudentFromSection conn sectionId studentId = do
  n <- execute conn "DELETE FROM sections_students WHERE section_id = ? AND student_id = ?" $ ((sectionId, studentId))
  return $ n > 0

-- Before applying this function YOU need to check if instructor is really added to this section
removeInstructorFromSection :: Connection -> Int64 -> Int64 -> IO(Bool)
removeInstructorFromSection conn sectionId instructorId = do
  n <- execute conn "DELETE FROM sections_instructors WHERE section_id = ? AND instructor_id = ?" $ ((sectionId, instructorId))
  return $ n > 0




getSectionStudent :: Connection -> Int64 -> Int64 -> IO[SectionStudent]
getSectionStudent conn sectionId studentId = query conn "SELECT * FROM sections_students \
                                                          \ WHERE section_id = ? AND student_id = ?"
                                                           $ ((sectionId, studentId))

getSectionInstructor :: Connection -> Int64 -> Int64 -> IO[SectionInstructor]
getSectionInstructor conn sectionId instructorId = query conn "SELECT * FROM sections_instructors \
                                                          \ WHERE section_id = ? AND instructor_id = ?"
                                                           $ ((sectionId, instructorId))


getAllInstructorsOfSection :: Connection -> Int64 -> IO[Instructor]
getAllInstructorsOfSection conn sectionId = query conn "SELECT instructors.id, name, surname, patronymic, birthday, degree \
                                                         \ FROM sections_instructors INNER JOIN instructors \
                                                         \ ON sections_instructors.instructor_id = instructors.id \
                                                         \ WHERE sections_instructors.section_id = ?"
                                                          $ (Only sectionId)


getAllStudentsOfSection :: Connection -> Int64 -> IO[Instructor]
getAllInstructorsOfSection conn sectionId = query conn "SELECT instructors.id, name, surname, patronymic, birthday, degree \
                                                         \ FROM sections_instructors INNER JOIN instructors \
                                                         \ ON sections_instructors.instructor_id = instructors.id \
                                                         \ WHERE sections_instructors.section_id = ?"
                                                          $ (Only sectionId)
  {-







  -}
-- need functions to remove from section