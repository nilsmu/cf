Public Class Arcade
    Function checkPalindrome(inputString As String) As Boolean
        Dim result As Boolean = True
        For i As Integer = 0 To inputString.Length / 2
            If Not inputString.Substring(i, 1).Equals(inputString.Substring(inputString.Length - i - 1, 1)) Then
                result = False
                Exit For
            End If
        Next
        Return result
    End Function

    Function maxMultiple(divisor As Integer, bound As Integer) As Integer
        Dim intResult As Integer = 1
        intResult = intResult * divisor
        While intResult <= (bound - divisor)
            intResult = intResult + divisor
        End While
        Return intResult
    End Function

    Function phoneCall(min1 As Integer, min2_10 As Integer, min11 As Integer, s As Integer) As Integer
        Dim intCosts As Integer
        Dim intMinutes As Integer = 0
        If min1 > s Then
            Return intMinutes
        Else
            intMinutes = intMinutes + 1
            intCosts = min1
            If intCosts + min2_10 > s Then
                Return intMinutes
            Else
                While intCosts + min2_10 <= s And intMinutes < 10
                    intCosts = intCosts + min2_10
                    intMinutes = intMinutes + 1
                End While
                If intCosts + min11 > s Then
                    Return intMinutes
                Else
                    While intCosts + min11 <= s And intMinutes >= 10
                        intCosts = intCosts + min11
                        intMinutes = intMinutes + 1
                    End While
                End If
            End If
        End If
        Return intMinutes
    End Function

    Function adjacentElementsProduct(inputArray As List(Of Integer)) As Integer
        Dim intMax As Integer = 0
        intMax = inputArray(0) * inputArray(1)
        For i As Integer = 1 To inputArray.Count - 2
            If inputArray(i) * inputArray(i + 1) > intMax Then intMax = inputArray(i) * inputArray(i + 1)
        Next
        Return intMax
    End Function

    Function makeArrayConsecutive2(statues As List(Of Integer)) As Integer
        Dim intMin As Integer
        Dim intMax As Integer
        Dim intCount As Integer = 0

        statues.Sort()

        intMin = statues.Item(0)
        intMax = statues.Item(statues.Count - 1)

        For i As Integer = intMin To intMax
            If Not statues.Contains(i) Then intCount += 1
        Next

        Return intCount
    End Function

    Function almostIncreasingSequence(sequence As List(Of Integer)) As Boolean
        Dim boolRemoved As Boolean = False

        For i As Integer = 0 To sequence.Count - 2
            If sequence(i) >= sequence(i + 1) Then
                If i > 0 AndAlso sequence(i + 1) <= sequence(i - 1) Then
                    sequence.RemoveRange(i + 1, 1)
                Else
                    sequence.RemoveRange(i, 1)
                End If
                boolRemoved = True
                Exit For
            End If
        Next

        If boolRemoved Then
            For i As Integer = 0 To sequence.Count - 2
                If sequence(i) >= sequence(i + 1) Then
                    Return False
                    Exit For
                End If
            Next
            Return True
        Else
            Return True
        End If
    End Function

    Function matrixElementsSum(matrix As List(Of List(Of Integer))) As Integer
        Dim intSum As Integer
        Dim intCol As Integer
        Dim intRow As Integer = 0
        Dim boolHaunted As Boolean = False

        For Each l As List(Of Integer) In matrix
            intCol = 0
            For Each i As Integer In l
                If intRow = 0 Then
                    intSum += matrix.Item(intRow).Item(intCol)
                Else
                    For n As Integer = intRow To 1 Step -1
                        If matrix.Item(n - 1).Item(intCol) = 0 Then
                            boolHaunted = True
                        End If
                    Next
                    If Not boolHaunted Then intSum += matrix.Item(intRow).Item(intCol)
                End If
                intCol += 1
                boolHaunted = False
            Next
            intRow += 1
        Next
        Return intSum
    End Function

    Function knapsackLight(value1 As Integer, weight1 As Integer, value2 As Integer, weight2 As Integer, maxW As Integer) As Integer
        If weight1 + weight2 <= maxW Then
            Return value1 + value2
        Else
            If value1 > value2 Then
                If weight1 <= maxW Then
                    Return value1
                Else
                    If weight2 <= maxW Then
                        Return value2
                    Else
                        Return 0
                    End If
                End If
            Else
                If weight2 <= maxW Then
                    Return value2
                Else
                    If weight1 <= maxW Then
                        Return value1
                    Else
                        Return 0
                    End If
                End If
            End If
        End If
    End Function

    Function isInfiniteProcess(a As Integer, b As Integer) As Boolean
        Dim boolResult As Boolean = False
        While a <> b
            If a > b Then
                boolResult = True
                Exit While
            End If
            a += 1
            b -= 1
        End While
        Return boolResult
    End Function

    Function tennisSet(score1 As Integer, score2 As Integer) As Boolean
        If (score1 = 7 And (score2 = 5 Or score2 = 6)) OrElse ((score1 = 5 Or score1 = 6) And score2 = 7) OrElse (score1 = 6 And score2 < 5) OrElse (score1 < 5 And score2 = 6) Then
            Return True
        Else
            Return False
        End If
    End Function

    Function willYou(young As Boolean, beautiful As Boolean, loved As Boolean) As Boolean
        Return IIf(loved, IIf((young And Not beautiful) OrElse (beautiful And Not young) OrElse (Not young And Not beautiful), True, False), IIf(young And beautiful, True, False))
    End Function

    Function metroCard(lastNumberOfDays As Integer) As List(Of Integer)
        Select Case lastNumberOfDays
            Case 28
                Return New List(Of Integer)(New Integer() {31})
            Case 30
                Return New List(Of Integer)(New Integer() {31})
            Case 31
                Return New List(Of Integer)(New Integer() {28, 30, 31})
            Case Else
                Return New List(Of Integer)
        End Select
    End Function

    Function allLongestStrings(inputArray As List(Of String)) As List(Of String)
        Dim intLongest As Integer = 0
        Dim strList As New List(Of String)
        For Each s As String In inputArray
            If s.Length > intLongest Then intLongest = s.Length
        Next
        For Each s As String In inputArray
            If s.Length = intLongest Then strList.Add(s)
        Next
        Return strList
    End Function

    Function commonCharacterCount(s1 As String, s2 As String) As Integer
        Dim intResult As Integer = 0
        Dim charList1 As New List(Of Char)
        Dim charList2 As New List(Of Char)
        For Each c As Char In s1
            charList1.Add(c)
        Next
        For Each c As Char In s2
            charList2.Add(c)
        Next
        For Each c As Char In charList1
            If charList2.Contains(c) Then
                intResult += 1
                charList2.RemoveRange(charList2.IndexOf(c), 1)
            End If
        Next
        Return intResult
    End Function

    Function isLucky(n As Integer) As Boolean
        Dim strNumber As String = CStr(n)
        Dim intHalf As Integer = strNumber.Length / 2
        Dim intFirst As Integer = 0
        Dim intSecond As Integer = 0
        For i As Integer = 0 To intHalf - 1
            intFirst += CInt(strNumber.Substring(i, 1))
        Next
        For i As Integer = strNumber.Length - 1 To intHalf Step -1
            intSecond += CInt(strNumber.Substring(i, 1))
        Next
        If intFirst = intSecond Then
            Return True
        Else
            Return False
        End If
    End Function

    Function sortByHeight(a As List(Of Integer)) As List(Of Integer)
        Dim loIntResult As New List(Of Integer)
        For Each i As Integer In a
            loIntResult.Add(i)
        Next
        While loIntResult.Contains(-1)
            loIntResult.Remove(-1)
        End While
        loIntResult.Sort()
        For i As Integer = 0 To a.Count - 1
            If a.Item(i) = -1 Then
                loIntResult.Insert(i, -1)
            End If
        Next
        Return loIntResult
    End Function

    Function reverseString(s As String) As String
        Dim res As String = ""
        For Each c As Char In s
            res = c & res
        Next
        Return res
    End Function

    Function hasParentheses(s As String) As Boolean
        Return (s.IndexOf("(") >= 0)
    End Function

    Sub findNextInnerMostParentheses(ByVal s As String, ByRef start As Integer, ByRef ende As Integer)
        start = s.IndexOf("(")
        Dim nexti As Integer = s.IndexOf("(", start + 1)
        ende = s.IndexOf(")")
        While (nexti >= 0) And (nexti < ende)
            start = nexti
            nexti = s.IndexOf("(", start + 1)
            ende = s.IndexOf(")", start + 1)
        End While
    End Sub

    Function reverseParentheses(s As String) As String
        Dim start As Integer = 0
        Dim ende As Integer = 0
        While hasParentheses(s)
            findNextInnerMostParentheses(s, start, ende)
            s = s.Substring(0, start) & reverseString(s.Substring(start + 1, ende - start - 1)) & s.Substring(ende + 1, s.Length - ende - 1)
        End While
        Return s
    End Function

    Function alternatingSums(a As List(Of Integer)) As List(Of Integer)
        Dim loIntResult As New List(Of Integer)
        Dim intWeight1 As Integer = 0
        Dim intWeight2 As Integer = 0

        For i As Integer = 0 To a.Count - 1
            If i Mod 2 = 0 Then
                intWeight1 += a.Item(i)
            Else
                intWeight2 += a.Item(i)
            End If
        Next
        loIntResult.Add(intWeight1)
        loIntResult.Add(intWeight2)
        Return loIntResult
    End Function

End Class
