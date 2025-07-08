# frozen_string_literal: true

################################################################################
#
# searchresultsorter.rb
#
# Sorter for search results
#
################################################################################
require 'rbfind/findsettings'

module RbSearch
  # SearchResultSorter - sorts search results
  class SearchResultSorter

    attr_reader :settings

    def initialize(settings)
      @settings = settings
    end

    def sort(search_results)
      search_result_comparator = get_search_result_comparator
      search_results.sort!(&search_result_comparator)
    end

    private

    def get_file_path_comparator
      if @settings.sort_descending
        if @settings.sort_case_insensitive
          ->(sr1, sr2) { sr2.cmp_by_path_ci(sr1) }
        else
          ->(sr1, sr2) { sr2.cmp_by_path(sr1) }
        end
      elsif @settings.sort_case_insensitive
        ->(sr1, sr2) { sr1.cmp_by_path_ci(sr2) }
      else
        ->(sr1, sr2) { sr1.cmp_by_path(sr2) }
      end
    end

    def get_file_name_comparator
      if @settings.sort_descending
        if @settings.sort_case_insensitive
          ->(sr1, sr2) { sr2.cmp_by_name_ci(sr1) }
        else
          ->(sr1, sr2) { sr2.cmp_by_name(sr1) }
        end
      elsif @settings.sort_case_insensitive
        ->(sr1, sr2) { sr1.cmp_by_name_ci(sr2) }
      else
        ->(sr1, sr2) { sr1.cmp_by_name(sr2) }
      end
    end

    def get_file_size_comparator
      if @settings.sort_descending
        if @settings.sort_case_insensitive
          ->(sr1, sr2) { sr2.cmp_by_size_ci(sr1) }
        else
          ->(sr1, sr2) { sr2.cmp_by_size(sr1) }
        end
      elsif @settings.sort_case_insensitive
        ->(sr1, sr2) { sr1.cmp_by_size_ci(sr2) }
      else
        ->(sr1, sr2) { sr1.cmp_by_size(sr2) }
      end
    end

    def get_file_type_comparator
      if @settings.sort_descending
        if @settings.sort_case_insensitive
          ->(sr1, sr2) { sr2.cmp_by_type_ci(sr1) }
        else
          ->(sr1, sr2) { sr2.cmp_by_type(sr1) }
        end
      elsif @settings.sort_case_insensitive
        ->(sr1, sr2) { sr1.cmp_by_type_ci(sr2) }
      else
        ->(sr1, sr2) { sr1.cmp_by_type(sr2) }
      end
    end

    def get_last_mod_comparator
      if @settings.sort_descending
        if @settings.sort_case_insensitive
          ->(sr1, sr2) { sr2.cmp_by_last_mod_ci(sr1) }
        else
          ->(sr1, sr2) { sr2.cmp_by_last_mod(sr1) }
        end
      elsif @settings.sort_case_insensitive
        ->(sr1, sr2) { sr1.cmp_by_last_mod_ci(sr2) }
      else
        ->(sr1, sr2) { sr1.cmp_by_last_mod(sr2) }
      end
    end

    def get_search_result_comparator
      case @settings.sort_by
      when RbFind::SortBy::FILENAME
        get_file_name_comparator
      when RbFind::SortBy::FILEPATH
        get_file_path_comparator
      when RbFind::SortBy::FILESIZE
        get_file_size_comparator
      when RbFind::SortBy::FILETYPE
        get_file_type_comparator
      when RbFind::SortBy::LASTMOD
        get_last_mod_comparator
      else
        get_file_path_comparator
      end
    end
  end
end
