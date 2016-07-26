# new notes

Queries return tables. This is nice because allows us to use the [Interpreter pattern](https://en.wikipedia.org/wiki/Interpreter_pattern).

For example, here's a min-stack:

    minStack = DataStructure.define do
      define :push do |item|
        table.insertAtEnd(item)
      end

      define :pop do
        res = table.itemAtEnd
        table.remove(res)
        res
      end

      define :get_min do
        table.sort_by { |x| x }.getFirst
      end
    end

Here's a list with getIndexOfMax

    myList = List.extend do
      define :getIndexOfMax
        table.find(_ == table.max)
      end
    end

Here's range minimum query:

    RMQ = ImmutableArray.extend do
      define :rmq do |x, y|
        table.slice(x, y).min
      end
    end

Here's a list with getTopK:

    stackWithGetTopK = Stack.extend do |k|
      define :getTopK do
        table.sort.take(k)
      end
    end

Here's a table which just supports arbitrary insert and getTopK:

    storeTopK = DataStructure.define do |k|
      def :insert do |item|
        table.insertSomewhere(item)
      end

      def :getTopK do
        table.sort.take(k)
      end
    end

"Design a data structure that supports the following operations: insert_back(), remove_front() and find_mode(), all in O(1)"

    DataStructure.define do
      define :insert_back do |item|
        table.insertAtEnd(item)
      end

      define :remove_front do
        res = table.first
        table.removeFromFront
        res
      end

      define :find_mode do
        table.group { |x| x.value }.sortBy { |x| x.count }.first.value
      end
    end
