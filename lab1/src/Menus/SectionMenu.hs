module Menus.SectionMenu where


import System.IO
import Database.PostgreSQL.Simple
import Entities.Section
import Entities.Student
import Entities.Instructor
import Entities.SectionLesson
import Data.Int
import Utils


sectionMainMenu :: Connection -> IO()
sectionMainMenu conn = do
  putStrLn("")
  putStrLn("======================= Section menu =======================")
  putStrLn("  1) Show section")
  putStrLn("  2) Show all sections")
  putStrLn("  3) Add section")
  putStrLn("  4) Edit section")
  putStrLn("  5) Delete section")
  putStrLn("  6) Instructors & Sections")
  putStrLn("  7) Students & Sections")
  putStrLn("  8) Sections lessons")
  putStrLn("  9) [Back]")
  putStrLn("============================================================")
  putStr("Option: ")
  hFlush stdout

  resp <- getChar
  _ <- getLine

  case resp of
    '1' -> showSection conn
    '2' -> showAllSections conn
    '3' -> showCreateSection conn
    '4' -> showEditSection conn
    '5' -> showDeleteSection conn
    '6' -> showInstructorSectionMenu conn
    '7' -> showStudentSectionMenu conn
    '8' -> showSectionLessonMenu conn
    _ -> putStrLn("")

  if resp /= '9'
   then sectionMainMenu conn
  else putStr("")


showSection :: Connection -> IO()
showSection conn = do
  putStr("Enter ID: ")
  hFlush stdout

  input <- getLine
  let id = (read input :: Int64)

  result <- getSection conn id
  if null(result) == True
    then putStrLn("Section with such ID does not exist")
  else mapM_ print result


showAllSections :: Connection -> IO()
showAllSections conn = do
  result <- getAllSections conn
  if null(result) == True
    then putStrLn("No section found")
  else mapM_ print result


showCreateSection :: Connection -> IO()
showCreateSection conn = do
  putStr("Title: ")
  hFlush stdout
  title <- getLine

  putStr("Description (one line): ")
  hFlush stdout
  description <- getLine

  createSection conn title description
  putStr("")


showEditSection :: Connection -> IO()
showEditSection conn = do
  putStr("Enter Section ID: ")
  hFlush stdout

  idStr <- getLine
  let id = (read idStr :: Int64)

  section <- getSection conn id
  if null(section) == True
    then putStrLn("Section with such ID does not exist")
  else do
    mapM_ print section

    putStrLn("")
    putStrLn("What do you want to edit?")
    putStrLn("  1) title")
    putStrLn("  2) description")
    putStrLn("  3) [back]")
    putStr("Option: ")
    hFlush stdout

    resp <- getChar
    _ <- getLine

    case resp of
      '1' -> do
        putStr("Title: ")
        hFlush stdout
        title <- getLine

        res <- updateSectionTitle conn id title
        if res == True
          then putStrLn("Updated successfully")
        else putStrLn("Not updated")


      '2' -> do
        putStr("Description: ")
        hFlush stdout
        description <- getLine

        res <- updateSectionDescription conn id description
        if res == True
          then putStrLn("Updated successfully")
        else putStrLn("Not updated")

      _ -> putStrLn("")


showDeleteSection :: Connection -> IO()
showDeleteSection conn = do
  putStr("Enter Section ID: ")
  hFlush stdout

  idStr <- getLine
  let id = (read idStr :: Int64)

  section <- getSection conn id
  if null(section) == True
    then putStrLn("Section with such ID does not exist")
  else do
    mapM_ print section

    putStr("Do you want to delete? (y/n) ")
    hFlush stdout

    resp <- getChar
    _ <- getLine

    if resp == 'y'
      then do
        res <- deleteSection conn id
        if res == True
          then putStrLn("Deleted successfully!")
        else putStrLn("Not deleted")
    else putStrLn("")


showInstructorSectionMenu :: Connection -> IO()
showInstructorSectionMenu conn = do
  putStrLn("")
  putStrLn("What do you want to do?")
  putStrLn("  1) Add instructor to section")
  putStrLn("  2) Get all instructors of section")
  putStrLn("  3) Remove instructor from section")
  putStrLn("  4) [back]")
  putStr("Option: ")
  hFlush stdout

  resp <- getChar
  _ <- getLine


  case resp of
    '1' -> do
      putStr("Enter Section ID: ")
      hFlush stdout
      sectionIdStr <- getLine
      let sectionId = (read sectionIdStr :: Int64)

      section <- getSection conn sectionId
      if null(section) == True
        then putStrLn("Section with such ID does not exist")
      else do
        putStr("Enter Instructor ID: ")
        hFlush stdout
        instructorIdStr <- getLine
        let instructorId = (read instructorIdStr :: Int64)

        instructor <- getInstructor conn instructorId
        if null(instructor) == True
          then putStrLn("Instructor with such ID does not exist")
        else do
          instructorInSection <- getSectionInstructor conn sectionId instructorId
          if null(instructorInSection) /= True
            then putStrLn("Given Instructor is already added to this Section")
          else do
            addInstructorToSection conn sectionId instructorId
            putStrLn("")

    '2' -> do
      putStr("Enter Section ID: ")
      hFlush stdout
      sectionIdStr <- getLine
      let sectionId = (read sectionIdStr :: Int64)

      section <- getSection conn sectionId
      if null(section) == True
        then putStrLn("Section with such ID does not exist")
      else do
        instructors <- getAllInstructorsOfSection conn sectionId
        if null(instructors) == True
          then putStrLn("No instructor found")
        else mapM_ print instructors

    '3' -> do
      putStr("Enter Section ID: ")
      hFlush stdout
      sectionIdStr <- getLine
      let sectionId = (read sectionIdStr :: Int64)

      section <- getSection conn sectionId
      if null(section) == True
        then putStrLn("Section with such ID does not exist")
      else do
        putStr("Enter Instructor ID: ")
        hFlush stdout
        instructorIdStr <- getLine
        let instructorId = (read instructorIdStr :: Int64)

        instructor <- getInstructor conn instructorId
        if null(instructor) == True
          then putStrLn("Instructor with such ID does not exist")
        else do
          instructorInSection <- getSectionInstructor conn sectionId instructorId
          if null(instructorInSection) == True
            then putStrLn("Given Instructor hasn't been added to this Section")
          else do
            removeInstructorFromSection conn sectionId instructorId
            putStrLn("")



    _ -> putStrLn("")



showStudentSectionMenu :: Connection -> IO()
showStudentSectionMenu conn = do
  putStrLn("")
  putStrLn("What do you want to do?")
  putStrLn("  1) Add student to section")
  putStrLn("  2) Get all students of section")
  putStrLn("  3) Remove student from section")
  putStrLn("  4) [back]")
  putStr("Option: ")
  hFlush stdout

  resp <- getChar
  _ <- getLine


  case resp of
    '1' -> do
      putStr("Enter Section ID: ")
      hFlush stdout
      sectionIdStr <- getLine
      let sectionId = (read sectionIdStr :: Int64)

      section <- getSection conn sectionId
      if null(section) == True
        then putStrLn("Section with such ID does not exist")
      else do
        putStr("Enter Student ID: ")
        hFlush stdout
        studentIdStr <- getLine
        let studentId = (read studentIdStr :: Int64)

        student <- getStudent conn studentId
        if null(student) == True
          then putStrLn("Student with such ID does not exist")
        else do
          studentInSection <- getSectionStudent conn sectionId studentId
          if null(studentInSection) /= True
            then putStrLn("Given Student is already added to this Section")
          else do
            addStudentToSection conn sectionId studentId
            putStrLn("")

    '2' -> do
      putStr("Enter Section ID: ")
      hFlush stdout
      sectionIdStr <- getLine
      let sectionId = (read sectionIdStr :: Int64)

      section <- getSection conn sectionId
      if null(section) == True
        then putStrLn("Section with such ID does not exist")
      else do
        students <- getAllStudentsOfSection conn sectionId
        if null(students) == True
          then putStrLn("No student found")
        else mapM_ print students

    '3' -> do
      putStr("Enter Section ID: ")
      hFlush stdout
      sectionIdStr <- getLine
      let sectionId = (read sectionIdStr :: Int64)

      section <- getSection conn sectionId
      if null(section) == True
        then putStrLn("Section with such ID does not exist")
      else do
        putStr("Enter Student ID: ")
        hFlush stdout
        studentIdStr <- getLine
        let studentId = (read studentIdStr :: Int64)

        student <- getStudent conn studentId
        if null(student) == True
          then putStrLn("Student with such ID does not exist")
        else do
          studentInSection <- getSectionStudent conn sectionId studentId
          if null(studentInSection) == True
            then putStrLn("Given Student hasn't been added to this Section")
          else do
            removeStudentFromSection conn sectionId studentId
            putStrLn("")



    _ -> putStrLn("")


showSectionLessonMenu :: Connection -> IO()
showSectionLessonMenu conn = do
  putStrLn("")
  putStrLn("What do you want to do?")
  putStrLn("  1) Add lesson")
  putStrLn("  2) Get all lessons of section")
  putStrLn("  3) Get all lessons")
  putStrLn("  4) Remove lesson")
  putStrLn("  5) [back]")
  putStr("Option: ")
  hFlush stdout

  resp <- getChar
  _ <- getLine


  case resp of
    '1' -> do
      putStr("Enter Section ID: ")
      hFlush stdout
      sectionIdStr <- getLine
      let sectionId = (read sectionIdStr :: Int64)

      section <- getSection conn sectionId
      if null(section) == True
        then putStrLn("Section with such ID does not exist")
      else do
        putStr("Date (yyyy-mm-dd): ")
        hFlush stdout
        dateStr <- getLine
        let date = parseDate dateStr

        putStr("Start time (hh:mm:ss): ")
        hFlush stdout
        startTimeStr <- getLine
        let startTime = parseTime startTimeStr

        putStr("Finish time (hh:mm:ss): ")
        hFlush stdout
        finishTimeStr <- getLine
        let finishTime = parseTime finishTimeStr

        createSectionLesson conn sectionId date startTime finishTime
        putStrLn("")

    '2' -> do
      putStr("Enter Section ID: ")
      hFlush stdout
      sectionIdStr <- getLine
      let sectionId = (read sectionIdStr :: Int64)

      section <- getSection conn sectionId
      if null(section) == True
        then putStrLn("Section with such ID does not exist")
      else do
        lessons <- getAllLessonsOfSection conn sectionId
        if null(lessons) == True
          then putStrLn("No lesson found")
        else mapM_ print lessons

    '3' -> do
      result <- getAllSectionLessons conn
      if null(result) == True
        then putStrLn("No lesson found")
      else mapM_ print result


    '4' -> do
      putStr("Enter Lesson ID: ")
      hFlush stdout
      lessonIdStr <- getLine
      let lessonId = (read lessonIdStr :: Int64)

      lesson <- getSectionLesson conn lessonId
      if null(lesson) == True
        then putStrLn("Lesson with such ID does not exist")
      else do
        mapM_ print lesson

        putStr("Do you want to delete? (y/n) ")
        hFlush stdout

        resp <- getChar
        _ <- getLine

        if resp == 'y'
          then do
            res <- deleteSectionLesson conn lessonId
            if res == True
              then putStrLn("Deleted successfully!")
            else putStrLn("Not deleted")
        else putStrLn("")



    _ -> putStrLn("")





