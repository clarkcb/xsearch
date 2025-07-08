namespace FsSearchLib

open FsFindLib

type SearchResultSorter (settings : SearchSettings) =
    
    member this.FileSorter = FileResultSorter(settings)

    member this.CompareByMatchLocation (r1 : SearchResult.t) (r2 : SearchResult.t) : int =
        let lineNumCmp = r1.LineNum - r2.LineNum
        if lineNumCmp = 0
        then
            let startIndexCmp = r1.MatchStartIndex - r2.MatchStartIndex
            if startIndexCmp = 0 then r1.MatchEndIndex - r2.MatchEndIndex else startIndexCmp
        else lineNumCmp

    member this.CompareByPath (r1 : SearchResult.t) (r2 : SearchResult.t) : int =
        let cmp = this.FileSorter.CompareByPath r1.File r2.File
        if cmp = 0 then this.CompareByMatchLocation r1 r2 else cmp

    member this.CompareByName (r1 : SearchResult.t) (r2 : SearchResult.t) : int =
        let cmp = this.FileSorter.CompareByName r1.File r2.File
        if cmp = 0 then this.CompareByMatchLocation r1 r2 else cmp

    member this.CompareBySize (r1 : SearchResult.t) (r2 : SearchResult.t) : int =
        let cmp = this.FileSorter.CompareBySize r1.File r2.File
        if cmp = 0 then this.CompareByMatchLocation r1 r2 else cmp

    member this.CompareByType (r1 : SearchResult.t) (r2 : SearchResult.t) : int =
        let cmp = this.FileSorter.CompareByType r1.File r2.File
        if cmp = 0 then this.CompareByMatchLocation r1 r2 else cmp

    member this.CompareByLastMod (r1 : SearchResult.t) (r2 : SearchResult.t) : int =
        let cmp = this.FileSorter.CompareByLastMod r1.File r2.File
        if cmp = 0 then this.CompareByMatchLocation r1 r2 else cmp

    member this.GetSearchResultComparator : SearchResult.t -> SearchResult.t -> int =
        if settings.SortDescending then
            match settings.SortBy with
            | SortBy.FileName -> (fun r1 r2 -> this.CompareByName r2 r1)
            | SortBy.FileSize -> (fun r1 r2 -> this.CompareBySize r2 r1)
            | SortBy.FileType -> (fun r1 r2 -> this.CompareByType r2 r1)
            | SortBy.LastMod  -> (fun r1 r2 -> this.CompareByLastMod r2 r1)
            | _               -> (fun r1 r2 -> this.CompareByPath r2 r1)
        else
            match settings.SortBy with
            | SortBy.FileName -> this.CompareByName
            | SortBy.FileSize -> this.CompareBySize
            | SortBy.FileType -> this.CompareByType
            | SortBy.LastMod  -> this.CompareByLastMod
            | _               -> this.CompareByPath

    member this.Sort (searchResults : SearchResult.t list) : SearchResult.t list = 
        let searchResultComparator = this.GetSearchResultComparator
        List.sortWith searchResultComparator searchResults

;;
