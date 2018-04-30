Public Class Interview
    Function firstDuplicate(a As List(Of Integer)) As Integer
        Dim intResult As Integer = -1
        Dim intList As New List(Of Integer)
        For i As Integer = 0 To a.Count - 1
            If intList.Contains(a.Item(i)) Then
                intResult = a.Item(i)
                Exit For
            Else
                intList.Add(a.Item(i))
            End If
        Next
        Return intResult
    End Function
End Class
