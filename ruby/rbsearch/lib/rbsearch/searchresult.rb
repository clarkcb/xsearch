# frozen_string_literal: true

module RbSearch

  # SearchResult - encapsulates a search result
  class SearchResult
    attr_accessor :pattern,
                  :file,
                  :line_num,
                  :match_start_index,
                  :match_end_index,
                  :line,
                  :lines_before,
                  :lines_after

    SEPARATOR_LEN = 80

    def initialize(pattern, file, line_num=0, match_start_index=0, match_end_index=0,
                   line='', lines_before=[], lines_after=[])
      @pattern = pattern
      @file = file
      @line_num = line_num
      @match_start_index = match_start_index
      @match_end_index = match_end_index
      @line = line
      @lines_before = lines_before
      @lines_after = lines_after
    end

    def cmp_by_search_fields(other)
      if @line_num == other.line_num
        if @match_start_index == other.match_start_index
          @match_end_index <=> other.match_end_index
        else
          @match_start_index <=> other.match_start_index
        end
      else
        @line_num <=> other.line_num
      end
    end

    def cmp_by_path(other)
      file_cmp = @file.cmp_by_path(other.file)
      if file_cmp.zero?
        cmp_by_search_fields(other)
      else
        file_cmp
      end
    end

    def cmp_by_path_ci(other)
      file_cmp = @file.cmp_by_path_ci(other.file)
      if file_cmp.zero?
        cmp_by_search_fields(other)
      else
        file_cmp
      end
    end

    def cmp_by_name(other)
      file_cmp = @file.cmp_by_name(other.file)
      if file_cmp.zero?
        cmp_by_search_fields(other)
      else
        file_cmp
      end
    end

    def cmp_by_name_ci(other)
      file_cmp = @file.cmp_by_name_ci(other.file)
      if file_cmp.zero?
        cmp_by_search_fields(other)
      else
        file_cmp
      end
    end

    def cmp_by_size(other)
      file_cmp = @file.cmp_by_size(other.file)
      if file_cmp.zero?
        cmp_by_search_fields(other)
      else
        file_cmp
      end
    end

    def cmp_by_size_ci(other)
      file_cmp = @file.cmp_by_size_ci(other.file)
      if file_cmp.zero?
        cmp_by_search_fields(other)
      else
        file_cmp
      end
    end

    def cmp_by_type(other)
      file_cmp = @file.cmp_by_type(other.file)
      if file_cmp.zero?
        cmp_by_search_fields(other)
      else
        file_cmp
      end
    end

    def cmp_by_type_ci(other)
      file_cmp = @file.cmp_by_type_ci(other.file)
      if file_cmp.zero?
        cmp_by_search_fields(other)
      else
        file_cmp
      end
    end

    def cmp_by_last_mod(other)
      file_cmp = @file.cmp_by_last_mod(other.file)
      if file_cmp.zero?
        cmp_by_search_fields(other)
      else
        file_cmp
      end
    end

    def cmp_by_last_mod_ci(other)
      file_cmp = @file.cmp_by_last_mod_ci(other.file)
      if file_cmp.zero?
        cmp_by_search_fields(other)
      else
        file_cmp
      end
    end

  end
end
